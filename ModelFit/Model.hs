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

module ModelFit.Model
    ( -- * Packing parameters
      Packed (..)
    , packed
    , ParamLoc
      -- * Building individual fits
    , FitExprT
    , FitExprM
    , FitExpr
    , param, fixed
    , paramExpr
    , fit
    , FitDesc (..)
    , fitEval
    , evalParam
    , freeParams
      -- * Building global fits
    , GlobalFitT
    , GlobalFitM
    , globalParam
    , expr, hoist
    , runGlobalFitT
    , runGlobalFitM
    , Curve(..)
      -- * Models
    , Model
    , liftOp
    ) where

import Prelude hiding (sequence, foldl, mapM, product)
import Data.Functor.Identity
import Data.Functor.Compose
import Control.Applicative
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.State (StateT (StateT), runStateT, evalStateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Traversable

import Control.Lens

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M

import ModelFit.FitExpr
import ModelFit.Types

vectorIx :: VG.Vector v a => Int -> Traversal' (v a) a
vectorIx i f v
  | 0 <= i && i < VG.length v = f (v VG.! i) <&> \a -> v VG.// [(i, a)]
  | otherwise                 = pure v

type Model param a = param a -> a -> a

newtype ParamIdx = ParamIdx Int
                 deriving (Show, Eq, Ord)

-- | A packed parameter vector
data Packed v a = Packed (v a)
                deriving (Show)
makeWrapped ''Packed

-- | A 'Lens' onto the value of the given parameter in the 'Packed' representation
packed :: VG.Vector v a => ParamIdx -> Lens' (Packed v a) a
packed (ParamIdx i) = singular $ unwrap . vectorIx i
  where
    unwrap = lens (\(Packed v)->v) (\_ v->Packed v)

-- | The location of a parameter and its initial value
data ParamLoc a = ParamLoc ParamIdx a

-- | The information necessary to compute a model over a curve
data Curve a = Curve { curvePoints :: VS.Vector (Point a)
                     , curveExpr   :: FitExpr (ParamLoc a) a (a -> a)
                     }

-- | A monad for defining a fit expression, with the ability to introduce
-- new parameters.
newtype FitExprT p m a = FEM (Compose (StateT [ParamIdx] m) (FitExpr (ParamLoc p) p) a)
                       deriving (Functor, Applicative)
        
type FitExprM p = FitExprT p Identity

instance MonadTrans (FitExprT p) where
    lift m = FEM $ Compose $ lift m >>= return . pure
    
instance (Functor m, Monad m) => Monad (FitExprT p m) where
    return = pure
    FEM (Compose a) >>= f = FEM $ Compose $ StateT $ \s->do
        res <- runStateT a s
        case res of
            (ra, s') -> runStateT f' s'
              where
                FEM (Compose f') = f b
                b = evalFitExpr (\(ParamLoc _ p)->p) ra

-- | Pop the first element of the state
popHead :: Monad m => StateT [s] m s
popHead = do
    x <- use $ singular _head
    id %= tail
    return x

paramExpr :: Monad m => p -> FitExprT p m (FitExpr (ParamLoc p) p p)
paramExpr initial = FEM $ Compose $ do
    idx <- popHead
    return $ return $ Param $ ParamLoc idx initial

-- TODO:
-- newParam:: a -> FitExprT 

-- | Introduce a new parameter to be optimized over (given an initial value)
param :: Monad m => a -> FitExprT a m a
param initial = FEM $ Compose $ do
    idx <- popHead
    return $ Param $ ParamLoc idx initial

-- | Lift a pure value into a fit expression
fixed :: (Monad m, Functor m) => a -> FitExprT p m a
fixed = pure

-- | Layout the parameters of the given expression
expr :: Monad m => FitExprT p m a -> GlobalFitT p m (FitExpr (ParamLoc p) p a)
expr (FEM m) = GFM $ lift $ getCompose m

-- | Hoist an expression into a @FitExprT@
hoist :: (Functor m, Monad m) => FitExpr (ParamLoc p) p a -> FitExprT p m a
hoist = FEM . Compose . pure

newtype GlobalFitT p m a = GFM (WriterT [Curve p] (StateT [ParamIdx] m) a)
                         deriving (Functor, Applicative, Monad)

type GlobalFitM p = GlobalFitT p Identity

instance MonadTrans (GlobalFitT p) where
    lift = GFM . lift . lift

-- | Create a global parameter
globalParam :: Monad m => s -> GlobalFitT s m (FitExprT s m s)
globalParam initial = GFM $ do
    idx <- lift popHead
    return $ FEM $ Compose $ return $ Param $ ParamLoc idx initial

data FitDesc a = FitDesc { fitModel  :: FitExpr (ParamLoc a) a (a -> a)
                         , fitPoints :: VS.Vector (Point a)
                         }

fitEval :: VS.Storable a => FitDesc a -> Packed VS.Vector a -> a -> a
fitEval fd params = evalFitExpr (\(ParamLoc p _) -> params ^. packed p) (fitModel fd)

-- | Add a curve to the fit, returning a function unpacking the
-- model parameters
fit :: (VS.Storable a, Monad m)
    => VS.Vector (Point a)       -- ^ Observed points
    -> FitExprT a m (a -> a)     -- ^ The model
    -> GlobalFitT a m (FitDesc a)
fit points (FEM m) = do
    m' <- GFM $ lift $ getCompose m
    GFM $ tell [Curve points m']
    return $ FitDesc { fitModel = m', fitPoints = points }

-- | Count the number of fitted parameters
freeParams :: FitExpr (ParamLoc p) p a -> Int
freeParams = M.size . packParams'

packParams' :: FitExpr (ParamLoc p) p a -> M.Map ParamIdx p
packParams' ps = go ps M.empty
  where
    go :: FitExpr (ParamLoc p) p a -> M.Map ParamIdx p -> M.Map ParamIdx p
    go (Param (ParamLoc i v)) m  = m & at i .~ Just v
    go (Pure _) m                = m
    go (ApT f a) m               = go f $ go a $ m
    go (Bind a g) m              = go (g $ evalFitExpr (\(ParamLoc _ v)->v) a) $ go a m

packParams :: (VS.Storable p, Num p) => FitExpr (ParamLoc p) p a -> VS.Vector p
packParams = go . packParams'
  where
    go m = VS.generate (n+1) pack
      where
        pack i = case M.lookup (ParamIdx i) m of
                     Just v  -> v
                     Nothing -> error $ "Couldn't find ParamIdx "++show i
        (ParamIdx n,_) = M.findMax m

evalParam :: (VS.Storable p) => FitExpr (ParamLoc p) p a -> Packed VS.Vector p -> a
evalParam e p = evalFitExpr (\(ParamLoc i _)->p ^. packed i) e

-- | Run a model definition
runGlobalFitT :: (VS.Storable s, Num s, Monad m)
              => GlobalFitT s m a
              -> m (a, [Curve s], Packed VS.Vector s)
runGlobalFitT (GFM action) = do
    res <- evalStateT (runWriterT action) (map ParamIdx [0..])
    case res of
      (r, curves) -> let p0 = packParams $ traverse curveExpr curves
                     in return (r, curves, Packed p0)

runGlobalFitM :: (VS.Storable s, Num s)
              => GlobalFitM s a
              -> (a, [Curve s], Packed VS.Vector s)
runGlobalFitM = runIdentity . runGlobalFitT

liftOp :: (a -> a -> a) -> (b -> a) -> (b -> a) -> (b -> a)
liftOp op f g = \x -> f x `op` g x
