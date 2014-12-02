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
             , ParamIdx (..)
             , Param (..)
             , Packed (..)
             , packed
               -- * Building parameter topologies
             , FitM
             , runFitM
             , param, fixed
             , fit
             , Curve(..)
               -- * Models
             , Model (..)
             , opModel, sumModel
             , constModel
             ) where

import Prelude hiding (sequence, foldl, mapM, product)
import Data.Monoid (Monoid (..))
import Control.Applicative
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.State (StateT, State, state, evalState, get, put)
import Control.Monad.Trans.Free
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

data Param a = Param ParamIdx a
             deriving (Functor, Foldable, Traversable, Show)

-- | A packed representation of a subset
data Packed v a = Packed (v a)
                deriving (Show)
makeWrapped ''Packed

-- | A @Lens@ onto the value of the given parameter in the @Packed@ representation
packed :: VG.Vector v a => Param a -> Lens' (Packed v a) a
packed (Param (ParamIdx i) _) = singular $ unwrap . vectorIx i
  where
    unwrap = lens (\(Packed v)->v) (\_ v->Packed v)

-- | The information necessary to compute a model over a curve
data Curve a = forall f. Curve { curvePoints :: VS.Vector (Point a)
                               , curveModel  :: Packed VS.Vector a -> a -> a
                               }

newtype FitExpr s a = FE (FreeT Param (State [ParamIdx]) a)
                    deriving (Functor, Applicative, Monad)

popHead :: Monad m => StateT [s] m s
popHead = do
    x <- use $ singular _head
    id %= tail
    return x

param :: VS.Storable a => a -> FitExpr a a
param initial = FE $ do
    idx <- lift popHead
    liftF $ Param idx initial

fixed :: a -> FitExpr s a
fixed = pure

-- The State Monad is to accommodate explicit sharing
newtype GlobalFitM s a = GFM (WriterT [Curve s] (State [ParamIdx]) a)
                       deriving (Functor, Applicative, Monad)

globalParam :: s -> GlobalFitM s (Param s)
globalParam initial = GFM $ do
    idx <- lift popHead
    return $ Param idx initial

-- | Add a curve to the fit, returning a function unpacking the
-- model parameters
fit :: (VS.Storable a)
    => VS.Vector (Point a)      -- ^ Observed points
    -> FitExpr a (a -> a)       -- ^ The model
    -> GlobalFitM a ()
fit points (FE m) = do
    m' <- GFM $ lift $ runFreeT m
    let runModel p = iter (\i->p ^. packed i) $ free m'
    GFM $ tell [Curve points runModel]

-- | Run a model definition
runGlobalFitM :: VS.Storable s
              => GlobalFitM s a
              -> ([Curve s], VS.Vector s, a)
runGlobalFitM (GFM action) = undefined
    --case evalState (runWriterT action) (map ParamIdx [0..]) of
      --(r, curves) -> (curves, r)

    
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
