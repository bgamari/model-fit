{-# LANGUAGE DataKinds, RankNTypes, FlexibleContexts #-}

import Constrained
import Control.Applicative
import Data.Traversable
import GHC.TypeLits
import Linear hiding (trace)
import Linear.V
import Numeric.AD
import Numeric.AD.Types
import Optimization.LineSearch.ConjugateGradient

import Debug.Trace
import Unsafe.Coerce
yes = unsafeCoerce  -- Until type inference on Nats works

--xmin :: (Show a, Show (f a), Additive f, Metric f, Traversable f, RealFloat a)
--     => FU f a -> f a -> [f a]
xmin (FU f) = trace "adsf" . conjGrad search beta (const 1) df --(tr . lowerFU f) df
  where df = tr . grad f
        search = backtrackingSearch 0.1 0.2
        beta = fletcherReeves

lmin :: (Show a, Show (f a), Additive f, Metric f, Traversable f, RealFloat a)
     => FU f a -> f a -> [f a]
lmin (FU f) = tr . conjGrad search beta (lowerFU f) df
  where df = grad f
        search = backtrackingSearch 0.1 0.2
        beta = fletcherReeves

tr x = traceShow x x

opt :: Opt V2 1 0 Double
opt = yes
      $ constrainEQ (\(V2 x y) -> traceShow ('g',x,y) $ x^2 + y^2)
      $ optimize (\(V2 x y) -> traceShow ('f',x,y) $ 2 - x^2 - 2*y^2)

x0 = V2 0 0
l0 = pure 1 :: V 1 Double

main = do
    let a :: [V2 Double]
        a = minimize xmin lmin opt x0 l0
    print $ head a
