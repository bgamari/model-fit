{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric,
    FlexibleInstances, FlexibleContexts, TypeFamilies,
    KindSignatures, DataKinds, TypeOperators, RankNTypes, ExistentialQuantification #-}

module Constrained
  ( -- * Building the problem
    Opt
  , FU(..)
  , optimize
  , constrainEQ
  , constrainLT
  , constrainGT
    -- * Optimizing the problem
  , minimize
    -- * Finding the Lagrangian
  , lagrangian
  ) where

import           Numeric.AD.Types

import           Data.Maybe
import           Control.Applicative
import           Linear.V
import           GHC.TypeLits
import qualified Data.Vector as V
import           Data.Traversable
import           Data.Foldable

newtype FU f a = FU { runFU :: forall s. Mode s => f (AD s a) -> AD s a }

-- | @Opt d f gs hs@ is a Lagrangian optimization problem with objective @f@
-- equality (@g(x) == 0@) constraints @gs@ and less-than (@h(x) < 0@)
-- constraints @hs@
data Opt f (kG::Nat) (kH::Nat) a =
    Opt (FU f a) (V kG (FU f a)) (V kH (FU f a))

optimize :: (forall s. Mode s => f (AD s a) -> AD s a) -> Opt f 0 0 a
optimize f = Opt (FU f) (pure $ FU undefined) (pure $ FU undefined)

--augment :: SingI (k+1) => a -> V k a -> V (k+1) a
--augment a = fromJust . fromVector . V.cons a . toVector

constrainEQ :: (forall s. Mode s => f (AD s a) -> AD s a)
            -> Opt f kG kH a -> Opt f (kG+1) kH a
constrainEQ g (Opt f gs hs) = Opt f (augment (FU g) gs) hs

constrainLT :: (forall s. Mode s => f (AD s a) -> AD s a)
            -> Opt f kG kH a -> Opt f kG (kH+1) a
constrainLT h (Opt f gs hs) = Opt f gs (augment (FU h) hs)

constrainGT :: (Num a) => (forall s. Mode s => f (AD s a) -> AD s a)
            -> Opt f kG kH a -> Opt f kG (kH+1) a
constrainGT h (Opt f gs hs) = Opt f gs (augment (FU $ negate . h) hs)

-- | Minimize the given constrained optimization problem
minimize :: (Functor f, Num a, SingI kG, SingI kH, g ~ V kG)
         => (FU f a -> f a -> [f a])   -- ^ Primal minimizer
         -> (FU g a -> g a -> [g a])   -- ^ Dual minimizer
         -> Opt f kG kH a              -- ^ The optimization problem of interest
         -> f a                        -- ^ The primal starting value
         -> g a                        -- ^ The dual starting value
         -> [f a]                      -- ^ Optimizing iterates
minimize minX minL opt = go
  where go x0 l0 = let l1 = head $ minL (FU $ \l -> -lagrangian opt (fmap auto x0) l) l0
                       x1 = head $ minX (FU $ \x ->  lagrangian opt x (fmap auto l1)) x0
                   in x1 : go x1 l1

-- | Maximize the given constrained optimization problem
maximize :: (Functor f, Num a, SingI kG, SingI kH, g ~ V kG)
         => (FU f a -> f a -> [f a])   -- ^ Primal minimizer
         -> (FU g a -> g a -> [g a])   -- ^ Dual minimizer
         -> Opt f kG kH a              -- ^ The optimization problem of interest
         -> f a                        -- ^ The primal starting value
         -> g a                        -- ^ The dual starting value
         -> [f a]                      -- ^ Optimizing iterates
maximize minX minL (Opt (FU f) gs hs) =
    minimize minX minL (Opt (FU $ negate . f) gs hs)

-- | The Lagrangian for the given constrained optimization
lagrangian :: (Num a) => Opt f kG kH a -> (forall s. Mode s => f (AD s a) -> V kG (AD s a) -> AD s a)
lagrangian (Opt (FU f) gs' hs') x l =
    f x - V.sum (V.zipWith (\lamb (FU g)->lamb * g x) ls gs)
  where ls = toVector l
        gs = toVector gs'
        hs = toVector hs'
