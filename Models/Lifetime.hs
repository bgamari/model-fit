{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Lifetime
    ( -- * Exponential decay
      LifetimeParams (..)
    , decayTime
    , amplitude
    , lifetimeModel
      -- * Instrument response
    , Irf
    , mkIrf
    , findPeriod
      -- * Convolution
    , Time
    , convolvedModel
    ) where

import Model
import Control.Lens
import Data.Foldable
import Data.Traversable       
import Data.Distributive
import Data.Distributive.Generic
import GHC.Generics (Generic1)
import qualified Data.Heap as Heap
import qualified Data.Vector.Storable as VS
import Data.Complex

import Numeric.FFT.Vector.Unitary
    
data  LifetimeParams a = LifetimeP { _decayTime :: !a
                                   , _amplitude :: !a
                                   }
                       deriving (Show, Functor, Foldable, Traversable, Generic1)
makeLenses ''LifetimeParams
instance Distributive LifetimeParams where distribute = genericDistribute

lifetimeModel :: RealFloat a => Model LifetimeParams a
lifetimeModel = Model $ \(LifetimeP taud amp) tau -> amp * exp (-tau / taud)
{-# INLINE lifetimeModel #-}

data Irf a = Irf { irfSamples :: !(VS.Vector a)
                 }

-- | Construct an IRF
mkIrf :: (VS.Storable a) => VS.Vector a -> Int -> Irf a
mkIrf samples period = Irf (VS.take period samples)

findPeriod :: (VS.Storable a, Num a, Ord a) => VS.Vector a -> Int
findPeriod corr = abs $ b - a
  where
    -- second-order finite difference
    dd = VS.zipWith3 (\a0 a1 a2 -> a0 - 2*a1 + a2) corr (VS.drop 1 corr) (VS.drop 2 corr)
    [a,b] = take 2 $ findMaxima $ flip zip [0..] $ VS.toList dd

findMaxima :: (Ord a) => [(a,b)] -> [b]
findMaxima = map Heap.payload . toList . Heap.fromList . fmap (uncurry Heap.Entry)

convolve :: Irf Double -> VS.Vector Double -> VS.Vector Double
convolve irf v = convolveValid (irfSamples irf) v
         
convolveValid :: VS.Vector Double -> VS.Vector Double -> VS.Vector Double
convolveValid a b = run dftC2R $ VS.zipWith (*) a' b'
  where
    a' = run dftR2C a
    b' = run dftR2C b

type Time = Double

convolvedModel :: Irf Double -> Int -> Time -> Model p Double -> Model p Double
convolvedModel irf nbins jiffy decay = Model f
  where
    f p = \x -> convolved VS.! round (x / jiffy)
      where
        convolved = convolve irf m
        m = let mp = model decay p
                bins = VS.enumFromTo 0 nbins :: VS.Vector Int
            in VS.map (\n->mp (realToFrac n * jiffy)) bins
