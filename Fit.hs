{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Fit where 

import Data.Functor.Identity
import Data.Foldable as F
import Data.Traversable as T
import Control.Applicative
import Data.Monoid (Monoid (..), Sum (..))

import Control.Lens
import qualified Data.Vector.Storable as V

import Linear
import Numeric.LevMar hiding (Model)
import Model

countParameters :: Foldable param => param a -> Int
countParameters = getSum . foldMap (const $ Sum 1)
{-# INLINE countParameters #-}

degreesOfFreedom :: (V.Storable a, Foldable param)
                 => V.Vector (Point a) -> param a -> Int
degreesOfFreedom pts param =
  V.length pts - countParameters param
{-# INLINE degreesOfFreedom #-}

chiSquared :: (Fractional a, Foldable f)
           => f (Point a) -> Model p a -> p a -> a
chiSquared pts m p = getSum $ foldMap go pts
  where
    go pt = Sum $ (residual pt (model m p))^2 / (pt ^. ptVar)

reducedChiSquared :: (V.Storable a, Fractional a, Foldable p)
                  => V.Vector (Point a) -> Model p a -> p a -> a
reducedChiSquared pts m p = chiSquared (V.toList pts) m p / realToFrac (degreesOfFreedom pts p)

leastSquares :: (Traversable p, V.Storable a, RealFloat a, LevMarable a)
             => p (Param a) -> [(V.Vector (Point a), Model p a)] -> Packed V.Vector p a
             -> Either LevMarError (Packed V.Vector p a)
leastSquares packing curves p0 =
    case levmar objective Nothing (p0 ^. _Wrapped) ys 5000 defaultOpts mempty of
      Left e -> Left e
      Right (p, _, _) -> Right $ Packed p
  where
    ys = V.concat $ map (V.map (const 0) . fst) curves
    objective packed = V.concat $ map (\(pts, Model m)->V.map (\(Point x y e) -> (m p x - y) / sqrt e) pts) curves
      where p = unpack packing (Packed packed)

residual :: Num a => Point a -> (a -> a) -> a
residual pt f = pt^._y - f (pt^._x)
