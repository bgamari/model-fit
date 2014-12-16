{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module ModelFit.Fit where

import Data.Foldable as F
import Data.Monoid (Monoid (..), Sum (..))

import Control.Lens
import qualified Data.Vector.Storable as VS

import Numeric.LevMar hiding (Model)
import ModelFit.Model
import ModelFit.Types

degreesOfFreedom :: (VS.Storable p, VS.Storable x, VS.Storable y)
                 => FitDesc p x y -> Int
degreesOfFreedom fd =
    VS.length (fitPoints fd) - freeParams (fitModel fd)
{-# INLINE degreesOfFreedom #-}

chiSquared' :: (Fractional y, Foldable f)
            => f (Point x y) -> (x -> y) -> y
chiSquared' pts model = getSum $ foldMap go pts
  where
    go pt = Sum $ (residual pt model)^2 / (pt ^. ptVar)

chiSquared :: (Fractional y, VS.Storable p, VS.Storable x, VS.Storable y)
           => FitDesc p x y -> Packed VS.Vector p -> y
chiSquared fd packed = chiSquared' (VS.toList $ fitPoints fd) (fitEval fd packed)

reducedChiSquared :: (VS.Storable p, VS.Storable x, VS.Storable y, Fractional y)
                  => FitDesc p x y -> Packed VS.Vector p -> y
reducedChiSquared fd packed =
    chiSquared fd packed / dof
  where
    dof = realToFrac $ degreesOfFreedom fd

leastSquares :: (y ~ p, VS.Storable x, VS.Storable p, RealFloat y, LevMarable y)
             => [Curve p x y]
             -> Packed VS.Vector p
             -> Either LevMarError (Packed VS.Vector p)
leastSquares curves p0 =
    case levmar objective Nothing (p0 ^. _Wrapped) ys 5000 defaultOpts mempty of
      Left e -> Left e
      Right (p, _, covar) -> Right $ Packed p
  where
    ys = VS.concat $ map (VS.map (const 0) . curvePoints) curves
    objective packed = VS.concat $ map doCurve curves
      where
        -- We first evaluate this to avoid repeating model evaluation if possible
        doCurve (Curve pts m) = VS.map (\(Point x y e) -> (mp x - y) / sqrt e) pts
          where
            mp = evalParam m (Packed packed)

residual :: Num y => Point x y -> (x -> y) -> y
residual pt f = pt^.ptY - f (pt^.ptX)
