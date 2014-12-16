{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module ModelFit.Model
    ( -- * Packing parameters
      Packed (..)
    , packed
    , ParamLoc
      -- * Building individual fits
    , FitExprT
    , FitExprM
    , FitExpr
    , param, fixed
    , fit
    , FitDesc (..)
    , fitEval
    , evalParam
    , freeParams
      -- * Building global fits
    , GlobalFitT
    , GlobalFitM
    , globalParam
    , expr, hoist
    , runGlobalFitT
    , runGlobalFitM
    , Curve(..)
      -- * Fit models
    , liftOp
      -- * Utilities
    , newParam
    , newGlobalParam
    , paramExpr
    , liftExpr
    ) where

import Prelude hiding (sequence, foldl, mapM, product)
import Data.Functor.Identity
import Data.Functor.Compose
import Control.Applicative
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.State (StateT (StateT), runStateT, evalStateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Traversable

import Control.Lens

import qualified Data.Vector.Storable as VS

import ModelFit.FitExpr
import ModelFit.Types
import ModelFit.Model.Packed

-- | The information necessary to compute a model over a curve
data Curve p x y = Curve { curvePoints :: VS.Vector (Point x y)
                         , curveExpr   :: FitExpr (ParamLoc p) p (x -> y)
                         }

-- | A monad for defining a fit expression, with the ability to introduce
-- new parameters.
newtype FitExprT p m a = FEM (Compose (StateT [ParamIdx] m) (FitExpr (ParamLoc p) p) a)
                       deriving (Functor, Applicative)
        
type FitExprM p = FitExprT p Identity

instance MonadTrans (FitExprT p) where
    lift m = FEM $ Compose $ lift m >>= return . pure
    
instance (Functor m, Monad m) => Monad (FitExprT p m) where
    return = pure
    FEM (Compose a) >>= f = FEM $ Compose $ StateT $ \s->do
        res <- runStateT a s
        case res of
            (ra, s') -> runStateT f' s'
              where
                FEM (Compose f') = f b
                b = evalFitExpr (\(ParamLoc _ p)->p) ra

-- | Pop the first element of the state
popHead :: Monad m => StateT [s] m s
popHead = do
    x <- use $ singular _head
    id %= tail
    return x

liftExpr :: Monad m => FitExpr (ParamLoc p) p a -> FitExprT p m a
liftExpr = FEM . Compose . return

newParam :: Monad m => p -> FitExprT p m (ParamLoc p)
newParam initial = FEM $ Compose $ do
    idx <- popHead
    return $ return $ ParamLoc idx initial

paramExpr :: (Functor m, Monad m) => p -> FitExprT p m (FitExpr (ParamLoc p) p p)
paramExpr initial = Param <$> newParam initial

-- | Introduce a new parameter to be optimized over (given an initial value)
param :: (Monad m, Functor m) => a -> FitExprT a m a
param initial = paramExpr initial >>= liftExpr

-- | Lift a pure value into a fit expression
fixed :: (Monad m, Functor m) => a -> FitExprT p m a
fixed = pure

-- | Layout the parameters of the given expression
expr :: Monad m => FitExprT p m a -> GlobalFitT p x y m (FitExpr (ParamLoc p) p a)
expr (FEM m) = GFM $ lift $ getCompose m

-- | Hoist an expression into a @FitExprT@
hoist :: (Functor m, Monad m) => FitExpr (ParamLoc p) p a -> FitExprT p m a
hoist = FEM . Compose . pure

newtype GlobalFitT p x y m a = GFM (WriterT [Curve p x y] (StateT [ParamIdx] m) a)
                             deriving (Functor, Applicative, Monad)

type GlobalFitM p x y = GlobalFitT p x y Identity

instance MonadTrans (GlobalFitT p x y) where
    lift = GFM . lift . lift

-- | Create a global parameter
newGlobalParam :: Monad m => p -> GlobalFitT p x y m (ParamLoc p)
newGlobalParam initial = GFM $ do
    idx <- lift popHead
    return $ ParamLoc idx initial

globalParam :: (Functor m, Monad m) => s -> GlobalFitT s x y m (FitExprT s m s)
globalParam initial = liftExpr . Param <$> newGlobalParam initial

data FitDesc p x y = FitDesc { fitModel  :: FitExpr (ParamLoc p) p (x -> y)
                             , fitPoints :: VS.Vector (Point x y)
                             }

fitEval :: VS.Storable p => FitDesc p x y -> Packed VS.Vector p -> x -> y
fitEval fd params = evalFitExpr (\(ParamLoc p _) -> params ^. packed p) (fitModel fd)

-- | Add a curve to the fit, returning a function unpacking the
-- model parameters
fit :: (VS.Storable p, Monad m)
    => VS.Vector (Point x y)       -- ^ Observed points
    -> FitExprT p m (x -> y)       -- ^ The model
    -> GlobalFitT p x y m (FitDesc p x y)
fit points (FEM m) = do
    m' <- GFM $ lift $ getCompose m
    GFM $ tell [Curve points m']
    return $ FitDesc { fitModel = m', fitPoints = points }

-- | Run a model definition
runGlobalFitT :: (VS.Storable p, Num p, Monad m)
              => GlobalFitT p x y m a
              -> m (a, [Curve p x y], Packed VS.Vector p)
runGlobalFitT (GFM action) = do
    res <- evalStateT (runWriterT action) (map ParamIdx [0..])
    case res of
      (r, curves) -> let p0 = packParams $ traverse curveExpr curves
                     in return (r, curves, Packed p0)

runGlobalFitM :: (VS.Storable p, Num p)
              => GlobalFitM p x y a
              -> (a, [Curve p x y], Packed VS.Vector p)
runGlobalFitM = runIdentity . runGlobalFitT

liftOp :: (a -> a -> a) -> (b -> a) -> (b -> a) -> (b -> a)
liftOp op f g = \x -> f x `op` g x
