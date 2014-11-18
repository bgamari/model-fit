{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module FcsFit where 

import Data.Functor.Identity
import Data.Foldable as F
import Data.Traversable as T
import Control.Applicative
import qualified Data.Vector as V
import Control.Lens

import Linear
import Optimization.LineSearch.ConjugateGradient
import Numeric.AD

import Model

import Debug.Trace

fit :: forall x y a param curves
     . ( Functor x
       , Additive y, Metric y, Functor y, Applicative y
       , Applicative curves, Foldable curves
       , RealFloat a, Show a
       , Functor param, Metric (PackedParams curves param)
       )
    => Model param x y                   -- ^ Model to fit
    -> curves (V.Vector (Point x y a))   -- ^ Curves to fit
    -> curves (param (ParamSource curves param a))  -- ^ Parameter sources
    -> PackedParams curves param a       -- ^ Initial parameters
    -> [curves (param a)]
fit m curves sources p0 =
    map (unpack sources)
    $ conjGrad search fletcherReeves df p0
  where
    --search = wolfeSearch 0.1 1 1e-4 0.9 f
    --search = armijoSearch 0.5 1 1e-4 f
    search = constantSearch 1e-3
    df :: Show a => PackedParams curves param a -> PackedParams curves param a
    df = tr . grad f
    --df = finiteDiff (fmap (const 1e-6) p0) f 
    f :: forall a. RealFloat a => PackedParams curves param a -> a
    f packed = chiSquared m
                (unpack (over (mapped . mapped . mapped) realToFrac sources) (over mapped realToFrac packed))
                (over (mapped . mapped . mapped) realToFrac curves)
  
tr x = traceShow x x
  
finiteDiff :: (Traversable f, Additive f, Metric f, RealFloat a)
           => f a -> (f a -> a) -> f a -> f a
finiteDiff h f x = fmap (\h'->(f (x ^+^ h') - f x) / norm h') (kronecker h)
