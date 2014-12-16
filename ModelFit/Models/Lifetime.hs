{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module ModelFit.Models.Lifetime
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

import Control.Lens
import Data.Foldable
import Data.Functor.Classes
import Data.Distributive
import Data.Distributive.Generic
import GHC.Generics (Generic1)
import qualified Data.Heap as Heap
import qualified Data.Vector.Storable as VS

import Numeric.FFT.Vector.Invertible

data  LifetimeParams a = LifetimeP { _decayTime :: !a
                                   , _amplitude :: !a
                                   }
                       deriving (Show, Functor, Foldable, Traversable, Generic1)
makeLenses ''LifetimeParams
instance Distributive LifetimeParams where distribute = genericDistribute


instance Show1 LifetimeParams where
    showsPrec1 _ (LifetimeP decay amp) a =
        "(Lifetime "++show decay++" "++show amp++")"++a

lifetimeModel :: RealFloat a => LifetimeParams a -> a -> a
lifetimeModel = \(LifetimeP taud amp) tau -> amp * exp (-tau / taud)
{-# INLINE lifetimeModel #-}

-- | A normalized response function
data Irf a = Irf { irfSamples :: !(VS.Vector a)
                 }
           deriving (Show)

-- | Construct an IRF. The function is automatically normalized.
mkIrf :: (Fractional a, VS.Storable a)
      => VS.Vector a -> Int -> Irf a
mkIrf samples period = Irf (VS.map (/ norm) v)
  where
    v = VS.take period samples
    norm = VS.sum v

findPeriod :: (VS.Storable a, Num a, Ord a) => VS.Vector a -> Int
findPeriod corr = abs $ b - a
  where
    -- second-order finite difference
    dd = VS.zipWith3 (\a0 a1 a2 -> a0 - 2*a1 + a2) corr (VS.drop 1 corr) (VS.drop 2 corr)
    [a,b] = take 2 $ findMaxima $ flip zip [0..] $ VS.toList dd

findMaxima :: (Ord a) => [(a,b)] -> [b]
findMaxima = map Heap.payload . toList . Heap.fromList . fmap (uncurry Heap.Entry)

convolve :: VS.Vector Double -> VS.Vector Double -> VS.Vector Double
convolve a b = run dftC2R $ VS.zipWith (*) a' b'
  where
    a' = run dftR2C a
    b' = run dftR2C b
{-# INLINEABLE convolve #-}

type Time = Double

-- | @convolvedModel irf nbins jiffy decay@ is a model described by
-- @decay$ convolved with @irf@. The model is valid over @x@ from
-- 0 to @jiffy * nbins@
convolvedModel :: Irf Double -> Int -> Time -> (Double -> Double) -> (Double -> Double)
convolvedModel irf nbins jiffy decayModel = f
  where
    paddedLength = nbins + VS.length (irfSamples irf) - 1
    --paddedLength = nbins
    paddedIrf = padTo paddedLength 0 (irfSamples irf)
    convolveWithIrf = convolve paddedIrf
    f x
      | i >= nbins = error $ "convolvedModel: x="++show x++" (bin "++show i++") requested but only computed "++show nbins++" bins."
      | otherwise  = convolved VS.! i
      where
        i = round $ x / jiffy
        convolved = convolveWithIrf (padTo paddedLength 0 m)
        m = let bins = VS.enumFromTo 0 nbins :: VS.Vector Int
            in VS.map (\n->decayModel $ realToFrac n * jiffy) bins
{-# INLINEABLE convolvedModel #-}

padTo :: VS.Storable a => Int -> a -> VS.Vector a -> VS.Vector a
padTo n x v = VS.concat [v, VS.replicate (n - VS.length v) x]
{-# INLINEABLE padTo #-}
