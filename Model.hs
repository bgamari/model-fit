{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
             , ParamsM
             , runParamsM
             , param, fixed, params
               -- * Models
             , Model (..)
             , opModel, sumModel
               -- * Parameters
             , ParamDesc
             , paramDescription
             , paramUnit
             ) where

import Prelude hiding (sequence, foldl, mapM, product)
import Data.Monoid (Monoid (..))
import Control.Applicative
import Control.Monad.Trans.State
import Data.Maybe
import Data.Foldable as F
import Data.Traversable
import Data.Functor.Product

import Control.Lens
import Linear

import qualified Data.Vector.Generic as VG
import qualified Data.Vector as V
import qualified Data.Map as M

vectorIx :: VG.Vector v a => Int -> Traversal' (v a) a
vectorIx i f v
  | 0 <= i && i < VG.length v = f (v VG.! i) <&> \a -> v VG.// [(i, a)]
  | otherwise                 = pure v

data Point x y a = Point { _ptX  :: !(x a)
                         , _ptY, _ptVar :: !(y a)
                         }
                 deriving (Show)
makeLenses ''Point

instance (Functor x, Functor y) => Functor (Point x y) where
    fmap f (Point x y var) = Point (fmap f x) (fmap f y) (fmap f var)

newtype ParamIdx = ParamIdx Int
                 deriving (Show, Eq, Ord)

data Param a = Param ParamIdx
             | Fixed a

newtype ParamsM a = PM (State Int a)
                  deriving (Functor, Applicative, Monad)

runParamsM :: ParamsM a -> a
runParamsM (PM action) = evalState action 0

param :: ParamsM (Param a)
param = PM $ state $ \n->(Param $ ParamIdx n, n+1)

fixed :: a -> ParamsM (Param a)
fixed = pure . Fixed

params :: (Applicative f, Traversable f) => ParamsM (f (Param a))
params = sequence $ pure param

-- Packing parameters

-- | A packed representation of a subset
data Packed v (f :: * -> *) a = Packed (v a)
                              deriving (Show)
makeWrapped ''Packed

{-
type instance Index (PackedParams curves param) = ParamIdx curves param
instance At (PackedParams curves param) where
    at = undefined
-}

embedParams :: (VG.Vector v a, Traversable f)
            => f (Param a) -> (forall b. Lens' (f b) b) -> Lens' (Packed v f a) a
embedParams idx l = singular $ _Wrapped' . vectorIx (mapping ^. l)
  where
    mapping = indexes idx

indexes :: (Traversable f) => f (Param a) -> f Int
indexes idx = evalState (mapM go idx) ([0..], M.empty)
  where
    go (Fixed x) = error "oh no"
    go (Param x) = do
      (next:rest, table) <- get
      case M.lookup x table of
        Just y -> return y
        Nothing -> do put $! (rest, M.insert x next table)
                      return next

unpack :: (VG.Vector v a, Traversable f)
       => f (Param a) -> Packed v f a -> f a
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


data ParamDesc a = PD { _paramDescription  :: String
                      , _paramUnit         :: String
                      }
                 deriving (Show)
makeLenses ''ParamDesc

data Model param a = Model { model :: param a -> a -> a }

opModel :: RealFloat a
        => (a -> a -> a) -> Model l a -> Model r a -> Model (Product l r) a
opModel op (Model a) (Model b) = Model $ \(Pair pa pb) x -> a pa x `op` b pb x

sumModel :: RealFloat y => Model a y -> Model b y -> Model (Product a b) y
sumModel = opModel (+)
