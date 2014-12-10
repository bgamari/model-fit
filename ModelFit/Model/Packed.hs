{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ModelFit.Model.Packed
    ( ParamIdx (..)
    , ParamLoc (..)
    , Packed (..)
    , packed
    , evalParam
    , packParams
    , freeParams
    ) where

import Control.Applicative

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M

import Control.Lens

import ModelFit.FitExpr

vectorIx :: VG.Vector v a => Int -> Traversal' (v a) a
vectorIx i f v
  | 0 <= i && i < VG.length v = f (v VG.! i) <&> \a -> v VG.// [(i, a)]
  | otherwise                 = pure v

newtype ParamIdx = ParamIdx Int
                 deriving (Show, Eq, Ord)

-- | The location of a parameter and its initial value
data ParamLoc a = ParamLoc ParamIdx a

-- | A packed parameter vector
data Packed v a = Packed (v a)
                deriving (Show)
makeWrapped ''Packed

-- | A 'Lens' onto the value of the given parameter in the 'Packed' representation
packed :: VG.Vector v a => ParamIdx -> Lens' (Packed v a) a
packed (ParamIdx i) = singular $ unwrap . vectorIx i
  where
    unwrap = lens (\(Packed v)->v) (\_ v->Packed v)

evalParam :: (VS.Storable p) => FitExpr (ParamLoc p) p a -> Packed VS.Vector p -> a
evalParam e p = evalFitExpr (\(ParamLoc i _)->p ^. packed i) e

packParams :: (VS.Storable p, Num p) => FitExpr (ParamLoc p) p a -> VS.Vector p
packParams = go . packParams'
  where
    go m = VS.generate (n+1) pack
      where
        pack i = case M.lookup (ParamIdx i) m of
                     Just v  -> v
                     Nothing -> error $ "Couldn't find ParamIdx "++show i
        (ParamIdx n,_) = M.findMax m

-- | Count the number of fitted parameters
freeParams :: FitExpr (ParamLoc p) p a -> Int
freeParams = M.size . packParams'

packParams' :: FitExpr (ParamLoc p) p a -> M.Map ParamIdx p
packParams' ps = go ps M.empty
  where
    go :: FitExpr (ParamLoc p) p a -> M.Map ParamIdx p -> M.Map ParamIdx p
    go (Param (ParamLoc i v)) m  = m & at i .~ Just v
    go (Pure _) m                = m
    go (ApT f a) m               = go f $ go a $ m
    go (Bind a g) m              = go (g $ evalFitExpr (\(ParamLoc _ v)->v) a) $ go a m
