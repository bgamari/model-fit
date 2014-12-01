{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
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
             , unpack
             , embedParams
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
import Control.Monad.RWS (RWS, runRWS, tell)
import Control.Monad.State (State, evalState, get, put)
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

data Param a = Param ParamIdx
             | Fixed a

-- | A packed representation of a subset
data Packed v a = Packed (v a)
                deriving (Show)
makeWrapped ''Packed

-- | The information necessary to compute a model over a curve            
data Curve a = forall f. Curve { curvePoints :: VS.Vector (Point a)
                               , curveModel  :: Packed VS.Vector a -> a -> a
                               }

newtype FitM s a = FM (RWS () [Curve s] ([ParamIdx], VS.Vector s) a)
                 deriving (Functor, Applicative, Monad)

popParamIdx :: FitM s ParamIdx
popParamIdx = FM $ do 
    idx <- use $ singular $ _1 . _head 
    _1 %= tail
    return idx

param :: VS.Storable a => a -> FitM a (Param a)
param initial = do
    idx <- popParamIdx
    FM $ _2 %= (`VG.snoc` initial)
    return $ Param idx

fixed :: a -> FitM s (Param a)
fixed = pure . Fixed

embedParams :: (VG.Vector v a, Traversable f)
            => f (Param a) -> (forall b. Lens' (f b) b) -> Lens' (Packed v a) a
embedParams idx l = singular $ _Wrapped' . vectorIx (mapping ^. l)
  where
    mapping = indexes idx

indexes :: (Traversable f) => f (Param a) -> f Int
indexes idx = evalState (mapM go idx) ([0..], M.empty)
  where
    go (Fixed x) = error "indexes: oh no"
    go (Param x) = do
      (next:rest, table) <- get
      case M.lookup x table of
        Just y -> return y
        Nothing -> do put $! (rest, M.insert x next table)
                      return next

unpack :: (VG.Vector v a, Traversable f)
       => f (Param a) -> Packed v a -> f a
unpack idx packed = evalState (mapM go idx) ([0..], M.empty)
  where
    go (Fixed x) = return x
    go (Param x) = do
      (next:rest, table) <- get
      i <- case M.lookup x table of
              Just i -> return i
              Nothing -> do put $! (rest, M.insert x next table)
                            return next
      return (packed ^. _Wrapped . to (VG.! i))
{-# INLINABLE unpack #-}

-- | Fit a curve against a model, returning a function unpacking the
-- model parameters
fit :: (VS.Storable a, Traversable f)
    => VS.Vector (Point a)       -- ^ Observed points
    -> Model f a                -- ^ The model
    -> f (FitM a (Param a))     -- ^ The model parameters
    -> FitM a (Packed VS.Vector a -> f a)
fit points m params = do
    packing <- sequence params
    let unpackParams = unpack packing
    FM $ tell [Curve points (model m . unpackParams)]
    return $ unpackParams

-- | Run a model definition
runFitM :: VS.Storable s
        => FitM s a -> ([Curve s], Packed VS.Vector s, a)
runFitM (FM action) =
    case runRWS action () (map ParamIdx [0..], VS.empty) of
      (r, (_, p0), curves) -> (curves, Packed p0, r)

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
