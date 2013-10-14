{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model ( -- * Data points
               Point(Point)
             , ptX, ptY, ptVar
               -- * Parameters
             , ParamDesc
             , paramDescription
             , paramUnit
               -- * Packing parameters
             , ParamIdx(..)
             , ParamSource(..)
             , PackedParams(..)
             , unpack
               -- * Models
             , Model(..)
             , product
             , chiSquared
             , chiSquared'
             , degreesOfFreedom
             ) where

import Linear
import Data.Monoid
import Control.Lens
import Control.Applicative
import Data.Maybe
import Data.Foldable as F
import Prelude hiding (product)
import qualified Data.Vector as V

data Point x y a = Point { _ptX  :: !(x a)
                         , _ptY, _ptVar :: !(y a)
                         }
                 deriving (Show)
makeLenses ''Point

newtype Model param x y a = Model { model :: param a -> x a -> y a }

newtype ParamIdx a
    = PIdx Int
    deriving (Show)

data ParamSource (curves :: * -> *) (param :: * -> *) a
    = Fixed a
    | FromVector (ParamIdx a)
    deriving (Show)

newtype PackedParams (curves :: * -> *) (param :: * -> *) a
    = PP (V.Vector a)
    deriving ( Show, Additive, Functor, Foldable, Traversable
             , Metric)
    
{-
type instance Index (PackedParams curves param) = ParamIdx curves param
instance At (PackedParams curves param) where
    at
-}

unpack :: (Functor curves, Functor param)
       => curves (param (ParamSource curves param a))
       -> PackedParams curves param a
       -> curves (param a)
unpack sources (PP packed) = fmap (fmap f) sources
  where
    f (Fixed a)             = a
    f (FromVector (PIdx i)) = fromMaybe (error "unpack: Invalid index") $ packed ^? ix i
{-# INLINE unpack #-}

chiSquared :: (RealFrac a, Applicative y, Metric y, Foldable curves, Applicative curves)
           => Model param x y a                -- ^ model
           -> curves (param a)                 -- ^ parameters
           -> curves (V.Vector (Point x y a))  -- ^ curves
           -> a                                -- ^ chi squared
chiSquared m params curves =
    getSum $ fold $ (\p curve->Sum $ chiSquared' m p curve) <$> params <*> curves
{-# INLINE chiSquared #-}

chiSquared' :: (RealFrac a, Applicative y, Metric y)
            => Model param x y a        -- ^ Model
            -> param a                  -- ^ Model parameters
            -> V.Vector (Point x y a)   -- ^ Curves
            -> a                        -- ^ Chi squared
chiSquared' (Model model) param curve = F.sum $ fmap f curve
  where f (Point x y var) = quadrance $ (/) <$> (model param x ^-^ y) <*> var
{-# INLINE chiSquared' #-}

countParameters :: Foldable param => param a -> Int
countParameters = getSum . foldMap (const $ Sum 1)
{-# INLINE countParameters #-}

degreesOfFreedom :: (Foldable param) => V.Vector (Point x y a) -> param a -> Int
degreesOfFreedom points param =
  V.length points - countParameters param
{-# INLINE degreesOfFreedom #-}

data ParamDesc a = PD { _paramDescription  :: String
                      , _paramUnit         :: String
                      }
                 deriving (Show)
makeLenses ''ParamDesc
