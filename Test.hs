{-# LANGUAGE DataKinds, RankNTypes, FlexibleContexts #-}

import Control.Applicative
import Data.Traversable
import GHC.TypeLits
import Linear hiding (trace)
import Numeric.AD
import Numeric.AD.Types
import Optimization.LineSearch.ConjugateGradient
import Optimization.LineSearch.SteepestDescent
import Optimization.Constrained.Penalty
import qualified Data.Vector as V

instance Metric V.Vector

xmin :: (Show a, Show (f a), Additive f, Metric f, Traversable f, RealFloat a)
     => FU f a -> f a -> [f a]
--xmin (FU f) = steepestDescent search (lowerFU f) df
xmin (FU f) = conjGrad search fletcherReeves (lowerFU f) df
  where df = grad f
        search = backtrackingSearch 1e-2 1e-3
        --search = constantSearch 1e-4

f (V2 x y) = 2 - x^2 - 2*y^2

opt :: Opt V2 Double
opt =   constrainGT (\(V2 x y) -> x^2 + y^2 - 1)
      $ optimize f

x0 = V2 1 10
l0 = V.replicate 1 1 :: V Double

main = do
    let a :: [V2 Double]
        a = maximize xmin opt 2 x0 l0
    mapM_ print $ take 1000 a
