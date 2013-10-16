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
import qualified Data.IntMap as IM
import Control.Lens

import Linear
import Optimization.LineSearch.ConjugateGradient
import Numeric.AD
import Numeric.AD.Types (Mode, AD)

import Model
import FcsModels

composeCore :: (Core f, Core g)
            => (Lens' (f (g a)) a -> b) -> f (g b)
composeCore f = core (\l->core (\m->f (l . m)))

fit :: forall x y a param curves
     . ( Functor x
       , Additive y, Metric y, Functor y, Applicative y
       , Applicative curves, Foldable curves
       , RealFloat a, Functor param, Metric (PackedParams curves param)
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
    --search = armijoSearch 0.1 2 1e-4 f
    search = constantSearch 0.01
    df :: PackedParams curves param a -> PackedParams curves param a
    df = grad f
    --df = finiteDiff (fmap (const 1e-6) p0) f 
    f :: forall a. RealFloat a => PackedParams curves param a -> a
    f packed = chiSquared m
                (unpack (fmap (fmap (fmap realToFrac)) sources) $ fmap realToFrac packed)
                (fmap (fmap (fmap realToFrac)) curves)
  
finiteDiff :: (Traversable f, Additive f, Metric f, RealFloat a)
           => f a -> (f a -> a) -> f a -> f a
finiteDiff h f x = fmap (\h'->(f (x ^+^ h') - f x) / norm h') (kronecker h)

main = do
    let genParams = defaultParams & (diffTime      .~ 10)
                                  . (concentration .~ 2)
        params = genParams
        m = diff3DModel
        points = V.fromList [ let x = 2**i in Point (V1 x) (model m genParams $ V1 x) (V1 1)
                            | i <- [1, 1.1..5] ]
        sources = Diff3DP (FromVector $ PIdx 0)
                          (Fixed 1)
                          (FromVector $ PIdx 1)
                          (FromVector $ PIdx 2)
        packedParams = PP $ IM.fromList $ zip [0..] [5,3,1]

    let p = Identity genParams in print $ (chiSquared m p (Identity points), runIdentity p)
    let fits = takeEvery 200 $ fit m (Identity points) (Identity sources) packedParams
    F.forM_ fits $ \p->do
        print (chiSquared m p (Identity points), runIdentity p)

takeEvery :: Int -> [a] -> [a]
takeEvery n (x:xs) = x : takeEvery n (drop n xs)
