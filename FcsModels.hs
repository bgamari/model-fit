{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module FcsModels ( Diff3DParams(..)
                 , diffTime
                 , diffExponent
                 , aspectRatio
                 , concentration
                 , diff3DModel
                 , defaultParams
                 ) where

import Model
import Linear
import Control.Lens
import Data.Foldable
import Data.Traversable       
import Data.Distributive
import Data.Distributive.Generic
import Data.Functor.Rep
import GHC.Generics (Generic1)

data Diff3DParams a = Diff3DP { _diffTime       :: !a
                              , _diffExponent   :: !a
                              , _aspectRatio    :: !a
                              , _concentration  :: !a
                              }
                    deriving (Show, Functor, Foldable, Traversable, Generic1)
makeLenses ''Diff3DParams
instance Distributive Diff3DParams where distribute = genericDistribute
instance Representable Diff3DParams

diff3DModel :: Model Diff3DParams V1 V1
diff3DModel = Model $ \(Diff3DP taud alpha a n) (V1 tau) ->
    let b = 1 + tau_taud
        c = 1 + tau_taud / a^2
        tau_taud = (tau / taud)**alpha
    in V1 $ 1 / b / sqrt c / n
{-# INLINE diff3DModel #-}

defaultParams :: Diff3DParams Double
defaultParams = Diff3DP 100 1 3 1
