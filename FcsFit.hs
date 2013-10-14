{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Functor.Identity
import Data.Foldable as F
import Data.Traversable as T
import Control.Applicative
import qualified Data.Vector as V
import Control.Lens

import Linear
import Optimization.LineSearch.ConjugateGradient

import Model
import FcsModels
                
composeCore :: (Core f, Core g)
            => (Lens' (f (g a)) a -> b) -> f (g b)
composeCore f = core (\l->core (\m->f (l . m)))

fit :: forall x y a param curves
     . ( Additive y, Metric y, Applicative y, Applicative curves, Foldable curves
       , RealFloat a, Functor param, Metric (PackedParams curves param)
       )
    => Model param x y a                 -- ^ Model to fit
    -> curves (V.Vector (Point x y a))   -- ^ Curves to fit
    -> curves (param (ParamSource curves param a))  -- ^ Parameter sources
    -> PackedParams curves param a       -- ^ Initial parameters
    -> [curves (param a)]
fit m curves sources p0 =
    map (unpack sources)
    $ conjGrad search fletcherReeves df p0
  where
    search = armijoSearch 0.1 1 1e-4 f
    df :: PackedParams curves param a -> PackedParams curves param a
    df = finiteDiff (pure 1e-3) f 
    f :: PackedParams curves param a -> a
    f packed = let p = unpack sources packed
               in chiSquared m ((,) <$> p <*> curves)
  
finiteDiff :: (Traversable f, Applicative f, Additive f, Num a)
           => f a -> (f a -> a) -> f a -> f a
finiteDiff h f x = fmap (\h'->f (x ^+^ h')) (kronecker h)

main = do
    let genParams = defaultParams & diffTime %~ (+10)
        params = genParams
        m = diff3DModel
        points = V.fromList [ Point (V1 x) (model m genParams $ V1 x) (V1 1)
                            | x <- [1e-6, 2e-6..100e-6] ]
        sources = fmap (FromVector . PIdx) $ Diff3DP 0 1 2 3
        packedParams = V.fromList [1,1,1,1]
    F.mapM_ (print . runIdentity)
      $ fit m (Identity points) (Identity sources) (PP packedParams)
