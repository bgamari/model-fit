{-# LANGUAGE RecordWildCards, MultiParamTypeClasses,
    DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric,
    FlexibleInstances, FlexibleContexts,
    KindSignatures, DataKinds, TypeOperators #-}

module FcsModels where

import           Linear
import           Control.Applicative
import           Data.Foldable as Foldable
import           Data.Traversable

class Model f a where
    model :: f a -> a -> a

-- | Kinetic model
data Kinetic a = KineticM { kG0   :: a
                          , kTauB :: a
                          }
                   deriving (Show, Eq, Functor, Foldable, Traversable)

instance Additive Kinetic where zero = pure 0
instance Metric Kinetic
instance Applicative Kinetic where
    pure a = KineticM a a
    KineticM a b <*> KineticM a' b' = KineticM (a a') (b b')

instance RealFloat a => Model Kinetic a where
    model (KineticM {..}) tau = kG0 * exp(-tau / kTauB)

-- | Diffusion model
data FcsModel a = Fcs { fcsTauD     :: a  -- ^ diffusion time (us)
                      , fcsA        :: a  -- ^ aspect ratio
                      , fcsG0       :: a  -- ^ amplitude
                      , fcsGinf     :: a  -- ^ offset
                      , fcsAlpha    :: a  -- ^ anomalous diffusion exponent
                      }
                deriving (Show, Eq, Functor, Foldable, Traversable)

instance Additive FcsModel where zero = pure 0
instance Metric FcsModel
instance Applicative FcsModel where
    pure a = Fcs a a a a a
    Fcs a b c d e <*> Fcs a' b' c' d' e' = Fcs (a a') (b b') (c c') (d d') (e e')

instance RealFloat a => Model FcsModel a where model = fcsCorr

fcsCorr :: (RealFloat a) => FcsModel a -> a -> a
fcsCorr (Fcs {..}) tau =
    fcsG0 / (1 + ttd) / sqrt (1 + 1/(fcsA*fcsA) * ttd) + fcsGinf
  where ttd = tau / fcsTauD

-- | Diffusion model with triplet
data FcsTriplet a = FcsT { fcsF    :: a
                         , fcsTauF :: a
                         , fcsFcs  :: FcsModel a
                         }
                  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Metric FcsTriplet
instance Additive FcsTriplet where zero = pure 0
instance Applicative FcsTriplet where
    pure a = FcsT a a (pure a)
    FcsT a b c <*> FcsT a' b' c'  = FcsT (a a') (b b') (c <*> c')

instance RealFloat a => Model FcsTriplet a where
    model = fcsTripletCorr

fcsTripletCorr :: (RealFloat a) => FcsTriplet a -> a -> a
fcsTripletCorr (FcsT {..}) tau =
    (1 - fcsF + fcsF * exp (-tau / fcsTauF)) / (1 - fcsF) * fcsCorr fcsFcs tau


-- | Sum model
data SumM f g a = SumM (f a) (g a)
                deriving (Show, Eq, Functor, Foldable, Traversable)

instance (Additive f, Applicative f, Additive g, Applicative g) => Additive (SumM f g) where zero = pure 0
instance (Metric f, Foldable f, Applicative f, Metric g, Foldable g, Applicative g) => Metric (SumM f g)
instance (Applicative f, Applicative g) => Applicative (SumM f g) where
    pure a = SumM (pure a) (pure a)
    SumM f g <*> SumM f' g' = SumM (f <*> f') (g <*> g')

instance (Model f a, Model g a, Num a) => Model (SumM f g) a where
    model (SumM fa ga) a = model fa a + model ga a

-- | Product model
data ProdM f g a = ProdM (f a) (g a)
                deriving (Show, Eq, Functor, Foldable, Traversable)

instance (Applicative f, Applicative g) => Applicative (ProdM f g) where
    pure a = ProdM (pure a) (pure a)
    ProdM f g <*> ProdM f' g' = ProdM (f <*> f') (g <*> g')

instance (Additive f, Applicative f, Additive g, Applicative g) => Additive (ProdM f g) where
    zero = pure 0

instance (Metric f, Foldable f, Applicative f, Metric g, Foldable g, Applicative g) =>
         Metric (ProdM f g)

instance (Model f a, Model g a, Num a) => Model (ProdM f g) a where
    model (ProdM fa ga) a = model fa a * model ga a
