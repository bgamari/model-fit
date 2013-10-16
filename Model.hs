{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

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
import qualified Data.IntMap as IM

data Point x y a = Point { _ptX  :: !(x a)
                         , _ptY, _ptVar :: !(y a)
                         }
                 deriving (Show)
makeLenses ''Point
           
instance (Functor x, Functor y) => Functor (Point x y) where
    fmap f (Point x y var) = Point (fmap f x) (fmap f y) (fmap f var)
    
data Model param x y = Model { model :: forall a. RealFloat a => param a -> x a -> y a }

newtype ParamIdx = PIdx Int
                 deriving (Show)

data ParamSource (curves :: * -> *) (param :: * -> *) a
    = Fixed a
    | FromVector ParamIdx
    deriving (Show)
    
instance Functor (ParamSource curves param) where
    fmap f (Fixed a)      = Fixed (f a)
    fmap f (FromVector n) = FromVector n

newtype PackedParams (curves :: * -> *) (param :: * -> *) a
    = PP (IM.IntMap a)
    deriving ( Show, Additive, Functor, Foldable, Traversable
             , Metric)
    
{-
type instance Index (PackedParams curves param) = ParamIdx curves param
instance At (PackedParams curves param) where
    at = undefined
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

chiSquared :: (RealFloat a, Applicative y, Metric y, Foldable curves, Applicative curves)
           => Model param x y                  -- ^ model
           -> curves (param a)                 -- ^ parameters
           -> curves (V.Vector (Point x y a))  -- ^ curves
           -> a                                -- ^ chi squared
chiSquared m params curves =
    getSum $ fold $ (\p curve->Sum $ chiSquared' m p curve) <$> params <*> curves
{-# INLINE chiSquared #-}

chiSquared' :: (RealFloat a, Applicative y, Metric y)
            => Model param x y          -- ^ Model
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
