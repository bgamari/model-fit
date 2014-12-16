module ModelFit.Model.Named
    ( -- * Packing parameters
      Packed
    , packed
    , ParamLoc
    , Params
      -- * Fit expressions
    , FitExpr
    , FitExprM
    , param
    , expr
    , hoist
      -- * Global fitting
    , GlobalFitM
    , globalParam
    , fit
    , fitEval
    , evalParam
    , runGlobalFitM
    , FitDesc
      -- * Helpers
    , liftOp
    ) where

import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M

import ModelFit.Model ( Packed, packed, FitDesc, Curve, fit, ParamLoc
                      , expr, hoist, liftOp, fitEval, evalParam)
import qualified ModelFit.Model as Mo
import ModelFit.FitExpr

type Params p = M.Map String (Mo.ParamLoc p)
type FitExprM p = Mo.FitExprT p (Writer (Params p))
type GlobalFitM p x y = Mo.GlobalFitT p x y (Writer (Params p))

globalParam :: String -> a -> GlobalFitM a x y (FitExprM a a)
globalParam name initial = do
    paramLoc <- Mo.newGlobalParam initial
    param <- Mo.expr $ Mo.liftExpr $ Param paramLoc
    lift $ tell $ M.singleton name paramLoc
    return $ Mo.liftExpr param

param :: String -> a -> FitExprM a a
param name initial = do
    paramLoc <- Mo.newParam initial
    lift $ tell $ M.singleton name paramLoc
    Mo.liftExpr $ Param paramLoc

runGlobalFitM :: (Num p, VS.Storable p)
              => GlobalFitM p x y a
              -> (a, [Curve p x y], Packed VS.Vector p, Params p)
runGlobalFitM action =
    let ((a,b,c), d) = runWriter $ Mo.runGlobalFitT action
    in (a,b,c,d)
