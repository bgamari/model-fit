{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module ModelFit.Types
    ( -- * Sample points
      Point (Point)
    , ptX, ptY, ptVar
    , pointV3
    , withVar
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Foreign.Storable
import Foreign.Ptr (castPtr, plusPtr)
import qualified Data.Vector as V

import Control.Lens
import Linear
import Data.Csv as Csv

data Point x y = Point { _ptX :: !x
                       , _ptY, _ptVar :: !y}
               deriving (Show)
makeLenses ''Point

pointV3 :: Iso' (Point a a) (V3 a)
pointV3 = iso rev fwd
  where
    fwd (V3 x y e) = Point x y e
    rev (Point x y e) = V3 x y e

-- | This doesn't respect the alignment requirements of y
instance (Storable x, Storable y) => Storable (Point x y) where
    sizeOf p = sizeOf (undefined :: x) + 2 * sizeOf(undefined :: y)
    alignment p = alignment (p ^. ptX) -- TODO: This isn't quite right
    peek ptr =
        Point <$> peek (castPtr ptr)
              <*> peek yPtr
              <*> peekElemOff yPtr 1
      where
        yPtr = ptr `plusPtr` sizeOf (undefined :: x)
    poke ptr (Point x y v) = do
        poke (castPtr ptr) x
        poke yPtr y
        pokeElemOff yPtr 1 v
      where
        yPtr = ptr `plusPtr` sizeOf (undefined :: x)

instance Functor (Point x) where
    fmap f (Point x y var) = Point x (f y) (f var)

instance FromRecord (Point Double Double) where
    parseRecord v
      | V.length v == 3 = Point <$> v Csv..! 0
                                <*> v Csv..! 1
                                <*> v Csv..! 2
      | otherwise       = mzero

withVar :: (a -> a) -> V2 a -> Point a a
withVar f (V2 x y) = Point x y (f y)
