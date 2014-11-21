{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import Control.Applicative
import Control.Monad (mzero)
import Foreign.Storable
import Foreign.Ptr (castPtr)
import qualified Data.Vector as V

import Control.Lens
import Linear
import Data.Csv as Csv

data Point a = Point { _ptX, _ptY, _ptVar :: !a}
             deriving (Show)
makeLenses ''Point

instance R1 Point where _x = ptX
instance R2 Point where _y = ptY

pointV3 :: Iso' (Point a) (V3 a)
pointV3 = iso rev fwd
  where
    fwd (V3 x y e) = Point x y e
    rev (Point x y e) = V3 x y e

instance (Storable a) => Storable (Point a) where
    sizeOf p = sizeOf (p ^. pointV3)
    alignment p = alignment (p ^. pointV3)
    peek ptr = (^. re pointV3) <$> peek (castPtr ptr)
    poke ptr p = poke (castPtr ptr) (p ^. pointV3)

instance Functor Point where
    fmap f (Point x y var) = Point (f x) (f y) (f var)

instance FromRecord (Point Double) where
    parseRecord v
      | V.length v == 3 = Point <$> v Csv..! 0
                                <*> v Csv..! 1
                                <*> v Csv..! 2
      | otherwise       = mzero

withVar :: (a -> a) -> V2 a -> Point a
withVar f (V2 x y) = Point x y (f y)
