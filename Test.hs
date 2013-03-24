{-# LANGUAGE DataKinds, RankNTypes, FlexibleContexts #-}

import Constrained
import Control.Applicative
import Data.Traversable
import GHC.TypeLits
import Linear hiding (trace)
import Numeric.AD
import Numeric.AD.Types
import Optimization.LineSearch.ConjugateGradient
import Optimization.LineSearch.SteepestDescent
import qualified Data.Vector as V

import Debug.Trace

instance Metric V.Vector

xmin :: (Show a, Show (f a), Additive f, Metric f, Traversable f, RealFloat a)
     => FU f a -> f a -> [f a]
xmin (FU f) = steepestDescent search (lowerFU f) df
  where df = grad f
        search = backtrackingSearch 0.1 0.2

lmin :: (Show a, Show (f a), Additive f, Metric f, Traversable f, RealFloat a)
     => FU f a -> f a -> [f a]
lmin (FU f) = steepestDescent search (lowerFU f) df
  where df = grad f
        search = backtrackingSearch 0.1 0.2

tr x = traceShow x x

f (V2 x y) = 2 - x^2 - 2*y^2

opt :: Opt V2 Double
opt =   constrainEQ (\(V2 x y) -> traceShow ('g',x,y) $ x^2 + y^2 - 1)
      $ optimize (\v@(V2 x y) -> traceShow ('f',x,y) $ f v)

x0 = V2 1 1
l0 = V.replicate 1 1 :: V Double

main = do
    let a :: [V2 Double]
        a = minimize xmin lmin opt x0 l0
    print $ take 3 a
