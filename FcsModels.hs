{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
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

data Diff3DParams a = Diff3DP { _diffTime       :: !a
                              , _diffExponent   :: !a
                              , _aspectRatio    :: !a
                              , _concentration  :: !a
                              }
                    deriving (Show, Functor, Foldable, Traversable)
makeLenses ''Diff3DParams

instance Core Diff3DParams where
    core f = Diff3DP (f diffTime) (f aspectRatio) (f diffExponent) (f concentration)

diff3DModel :: RealFloat a => Model Diff3DParams V1 V1 a
diff3DModel = Model $ \(Diff3DP taud a alpha n) (V1 tau) ->
    let b = 1 + tau_taud
        c = 1 + tau_taud / a^2
        tau_taud = (tau / (taud))**alpha
    in V1 $ 1 / b / sqrt c / n

defaultParams :: Diff3DParams Double
defaultParams = Diff3DP 100 1 3 1
