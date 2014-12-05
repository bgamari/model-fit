{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module ModelFit.Fit where

import Data.Functor.Identity
import Data.Foldable as F
import Data.Traversable as T
import Control.Applicative
import Data.Monoid (Monoid (..), Sum (..))

import Control.Lens
import qualified Data.Vector.Storable as VS

import Linear
import Numeric.LevMar hiding (Model)
import ModelFit.Model

degreesOfFreedom :: (VS.Storable a)
                 => FitDesc a -> Int
degreesOfFreedom fd =
    VS.length (fitPoints fd) - freeParams (fitModel fd)
{-# INLINE degreesOfFreedom #-}

chiSquared' :: (Fractional a, Foldable f)
            => f (Point a) -> (a -> a) -> a
chiSquared' pts model = getSum $ foldMap go pts
  where
    go pt = Sum $ (residual pt model)^2 / (pt ^. ptVar)

chiSquared :: (Fractional a, VS.Storable a) => FitDesc a -> Packed VS.Vector a -> a
chiSquared fd packed = chiSquared' (VS.toList $ fitPoints fd) (fitEval fd packed)

reducedChiSquared :: (VS.Storable a, Fractional a)
                  => FitDesc a -> Packed VS.Vector a -> a
reducedChiSquared fd packed =
    chiSquared fd packed / dof
  where
    dof = realToFrac $ degreesOfFreedom fd

leastSquares :: (VS.Storable a, RealFloat a, LevMarable a)
             => [Curve a] -> Packed VS.Vector a
             -> Either LevMarError (Packed VS.Vector a)
leastSquares curves p0 =
    case levmar objective Nothing (p0 ^. _Wrapped) ys 5000 defaultOpts mempty of
      Left e -> Left e
      Right (p, _, _) -> Right $ Packed p
  where
    ys = VS.concat $ map (VS.map (const 0) . curvePoints) curves
    objective packed = VS.concat $ map doCurve curves
      where
        -- We first evaluate this to avoid repeating model evaluation if possible
        doCurve (Curve pts m) = VS.map (\(Point x y e) -> (mp x - y) / sqrt e) pts
          where
            mp = evalParam m (Packed packed)

residual :: Num a => Point a -> (a -> a) -> a
residual pt f = pt^._y - f (pt^._x)
