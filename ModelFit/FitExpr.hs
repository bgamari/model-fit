{-# LANGUAGE GADTs #-}

module ModelFit.FitExpr
    ( FitExpr (..)
    , evalFitExpr
    , mapIndex
    ) where

import Control.Applicative

-- | A fit expression
--
-- This is an expression of type @a@ which can have within it
-- parameters of type @p@ from an external source indexed by @i@.
data FitExpr i p a where
    Param :: !i -> FitExpr i p p
    Pure  :: !a -> FitExpr i p a
    ApT   :: FitExpr i p (a -> b) -> FitExpr i p a -> FitExpr i p b -- TODO: remove
    Bind  :: FitExpr i p a -> (a -> FitExpr i p b) -> FitExpr i p b

instance Functor (FitExpr i p) where
  fmap f (Param i) = ApT (pure f) (Param i)
  fmap f (Pure a) = Pure (f a)
  fmap f (ApT g a) = ApT (ApT (Pure (f .)) g) a
  fmap f (Bind a g) = Bind a (fmap f . g)

instance Applicative (FitExpr i p) where
  pure = Pure
  f <*> x = ApT f x

instance Monad (FitExpr i p) where
  return = Pure
  a >>= f = Bind a f

-- | Evaluate the fit expression given a parameter vector
evalFitExpr :: (i -> p) -> FitExpr i p a -> a
evalFitExpr _ (Pure a)   = a
evalFitExpr f (Param i)  = f i
evalFitExpr f (ApT g a)  = (evalFitExpr f g) (evalFitExpr f a)
evalFitExpr f (Bind a g) = evalFitExpr f (g (evalFitExpr f a))

mapIndex :: (i -> j) -> FitExpr i p a -> FitExpr j p a
mapIndex _ (Pure a)   = Pure a
mapIndex f (Param i)  = Param (f i)
mapIndex f (ApT g a)  = ApT (mapIndex f g) (mapIndex f a)         
mapIndex f (Bind a g) = Bind (mapIndex f a) (mapIndex f . g)

--traverseParams :: Applicative f
--               => (i -> p) -> (i -> f j) -> FitExpr i p a -> f (FitExpr j p b)
--traverseParams _   _ (Pure a)   = pure $ Pure a
--traverseParams _   f (Param i)  = Param <$> f i
--traverseParams get f (ApT g a)  = ApT <$> traverseParams get f g <*> traverseParams get f a
--traverseParams get f (Bind a g) = Bind <$> traverseParams f a <*> (traverseParams f $ g (evalFitExpr get a))
