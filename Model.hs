{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module Model ( -- * Data samples
               Point (Point)
             , ptX, ptY, ptVar
               -- * Packing parameters
             , Packed (..)
             , packed
               -- * Building individual fits
             , FitExpr
             , FitExprM
             , param, fixed
             , fit
             , evalParam, evalDefault
               -- * Building global fits
             , GlobalFitM
             , globalParam
             , expr, hoist
             , runGlobalFitM
             , Curve(..)
               -- * Models
             , Model (..)
             , opModel, sumModel
             , constModel
             ) where

import Prelude hiding (sequence, foldl, mapM, product)
import Data.Monoid (Monoid (..))
import Data.Functor.Compose
import Control.Applicative
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.State (StateT, State, state, evalState, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Maybe
import Data.Foldable as F
import Data.Traversable
import Data.Functor.Product
import Data.Functor.Identity

import Control.Lens
import Linear

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M

import Types

vectorIx :: VG.Vector v a => Int -> Traversal' (v a) a
vectorIx i f v
  | 0 <= i && i < VG.length v = f (v VG.! i) <&> \a -> v VG.// [(i, a)]
  | otherwise                 = pure v

newtype Model param a = Model { model :: param a -> a -> a }

newtype ParamIdx = ParamIdx Int
                 deriving (Show, Eq, Ord)

-- | A packed parameter vector
data Packed v a = Packed (v a)
                deriving (Show)
makeWrapped ''Packed

-- | A @Lens@ onto the value of the given parameter in the @Packed@ representation
packed :: VG.Vector v a => ParamIdx -> Lens' (Packed v a) a
packed (ParamIdx i) = singular $ unwrap . vectorIx i
  where
    unwrap = lens (\(Packed v)->v) (\_ v->Packed v)

-- | The information necessary to compute a model over a curve
data Curve a = forall f. Curve { curvePoints :: VS.Vector (Point a)
                               , curveExpr   :: FitExpr a (a -> a)
                               }

newtype FitExprM s a = FEM (Compose (State [ParamIdx]) (FitExpr s) a)
                     deriving (Functor, Applicative)

-- | A fit expression
data FitExpr s a where
    Param :: ParamIdx -> s -> FitExpr s s
    Pure  :: a -> FitExpr s a
    ApT   :: FitExpr s (a -> b) -> FitExpr s a -> FitExpr s b -- TODO: remove
    Bind  :: FitExpr s a -> (a -> FitExpr s b) -> FitExpr s b

instance Functor (FitExpr s) where
  fmap f (Param n a) = ApT (pure f) (Param n a)
  fmap f (Pure a) = Pure (f a)
  fmap f (ApT g a) = ApT (ApT (Pure (f .)) g) a
  fmap f (Bind a g) = Bind a (fmap f . g)

instance Applicative (FitExpr s) where
  pure = Pure
  f <*> x = ApT f x

instance Monad (FitExpr s) where
  return = Pure
  a >>= f = Bind a f

popHead :: Monad m => StateT [s] m s
popHead = do
    x <- use $ singular _head
    id %= tail
    return x

-- | Create a parameter to be optimized over (given an initial value)
param :: VS.Storable a => a -> FitExprM a a
param initial = FEM $ Compose $ do
    idx <- popHead
    return $ Param idx initial

-- | Lift a pure value into a fit expression
fixed :: a -> FitExprM s a
fixed = pure

-- | Layout the parameters of the given expression
expr :: FitExprM s a -> GlobalFitM s (FitExpr s a)
expr (FEM m) = do
    GFM $ lift $ getCompose m

-- | Hoist an expression into a @FitExprM@
hoist :: FitExpr s a -> FitExprM s a
hoist = FEM . Compose . pure

newtype GlobalFitM s a = GFM (WriterT [Curve s] (State [ParamIdx]) a)
                       deriving (Functor, Applicative, Monad)

-- | Create a global parameter
globalParam :: s -> GlobalFitM s (FitExprM s s)
globalParam initial = GFM $ do
    idx <- lift popHead
    return $ FEM $ Compose $ return $ Param idx initial

-- | Add a curve to the fit, returning a function unpacking the
-- model parameters
fit :: (VS.Storable a)
    => VS.Vector (Point a)      -- ^ Observed points
    -> FitExprM a (a -> a)       -- ^ The model
    -> GlobalFitM a ()
fit points (FEM m) = do
    m' <- GFM $ lift $ getCompose m
    GFM $ tell [Curve points m']

-- | Evaluate the fit expression given the initial values
evalDefault :: FitExpr s a -> a
evalDefault (Pure a) = a
evalDefault (Param _ a) = a
evalDefault (ApT f a) = (evalDefault f) (evalDefault a)
evalDefault (Bind a f) = evalDefault $ f (evalDefault a)

-- | Evaluate the fit expression given a parameter vector
evalParam :: VS.Storable s => FitExpr s a -> Packed VS.Vector s -> a
evalParam (Pure a) _ = a
evalParam (Param (ParamIdx i) _) (Packed v) = v VS.! i
evalParam (ApT f a) v = (evalParam f v) (evalParam a v)
evalParam (Bind a f) v = evalParam (f (evalParam a v)) v

packParams' :: FitExpr s a -> M.Map ParamIdx s
packParams' ps = go ps M.empty
  where
    go :: FitExpr s a -> M.Map ParamIdx s -> M.Map ParamIdx s
    go (Param i a) m = m & at i .~ Just a
    go (Pure _) m = m
    go (ApT f a) m = go a $ go f m
    go (Bind a g) m = go (g $ evalDefault a) $ go a m

packParams :: (VS.Storable s, Num s) => FitExpr s a -> VS.Vector s
packParams = go . packParams'
  where
    go m = foldl' (\s (ParamIdx i, x)->s & ix i .~ x) (VS.replicate (n+1) 0) (M.assocs m)
      where
        (ParamIdx n,_) = M.findMax m

-- | Run a model definition
runGlobalFitM :: (VS.Storable s, Num s)
              => GlobalFitM s a
              -> ([Curve s], Packed VS.Vector s, a)
runGlobalFitM (GFM action) = do
    case evalState (runWriterT action) (map ParamIdx [0..]) of
      (r, curves) -> let p0 = packParams $ traverse curveExpr curves
                     in (curves, Packed p0, r)

opModel :: RealFloat a
        => (a -> a -> a) -> Model l a -> Model r a -> Model (Product l r) a
opModel op (Model a) (Model b) =
    Model $ \(Pair pa pb) -> let a' = a pa
                                 b' = b pb
                             in \x -> a' x `op` b' x
{-# INLINEABLE opModel #-}

sumModel :: RealFloat y => Model a y -> Model b y -> Model (Product a b) y
sumModel = opModel (+)
{-# INLINEABLE sumModel #-}

constModel :: Model Identity a
constModel = Model $ \(Identity p) _ -> p
{-# INLINEABLE constModel #-}
