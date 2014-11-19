{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module FcsFit where 

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
                 => V.Vector (V2 a) -> param a -> Int
degreesOfFreedom points param =
  V.length points - countParameters param
{-# INLINE degreesOfFreedom #-}

chiSquared :: (Num a, Foldable f)
           => f (V2 a) -> Model p a -> p a -> a
chiSquared pts m p = getSum $ foldMap (\pt->Sum $ residual pt $ model m p) pts

leastSquares :: (Traversable p, V.Storable a, RealFloat a, LevMarable a)
             => V.Vector (V2 a) -> p (Param a) -> Model p a -> Packed V.Vector p a
             -> Either LevMarError (Packed V.Vector p a)
leastSquares pts packing (Model m) p0 =
    case levmar model Nothing (p0 ^. _Wrapped) (V.map (^. _y) pts) 5000 defaultOpts mempty of
      Left e -> Left e
      Right (p, _, _) -> Right $ Packed p
  where
    model packed = V.map (\(V2 x y) -> m p x - y) pts
      where p = unpack packing (Packed packed)

residual :: Num a => V2 a -> (a -> a) -> a
residual pt f = pt^._y - f (pt^._x)
