module ModelFit.Model.Named
    ( -- * Packing parameters
      Packed
    , packed
    , ParamLoc
    , Params
      -- * Fit expressions
    , FitExprM
      -- * Global fitting
    , GlobalFitM
    , globalParam
    , fit
    , runGlobalFitM
      -- * Fit expressions
    , FitExpr
    ) where

import Data.Monoid
import Control.Monad.Trans.Writer (WriterT(..), runWriterT, tell)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M

import ModelFit.Model (Packed, packed, FitDesc, Curve)
import qualified ModelFit.Model as Mo
import ModelFit.Types

type Params s = M.Map String (Mo.FitExpr (Mo.ParamLoc s) s s)
type FitExprM s = WriterT (Params s) (Mo.FitExprM s)
type GlobalFitM s = WriterT (Params s) (Mo.GlobalFitM s) 

globalParam :: String -> a -> GlobalFitM a (FitExprM a a)
globalParam name initial = do
    p <- lift $ Mo.globalParam initial >>= Mo.expr
    tell $ M.singleton name p
    --return $ WriterT $ fmap (\a->(a, mempty)) (Mo.hoist p)
    lift p

param :: String -> a -> FitExprM a a
param name initial = do
    p <- lift $ Mo.paramExpr initial
    tell $ M.singleton name p
    WriterT $ fmap (\a->(a, mempty)) $ Mo.hoist p

fit :: (VS.Storable a)
    => VS.Vector (Point a)
    -> FitExprM a (a -> a)
    -> GlobalFitM a (FitDesc a)
fit points action = do
    (expr, params) <- Mo.expr $ lift $ runWriterT action
    tell params
    lift $ Mo.fit points expr

runGlobalFitM :: GlobalFitM s a -> ([Curve s], Packed VS.Vector s, Params s, a)
runGlobalFitM action = Mo.runGlobalFitM $ runWriterT action
