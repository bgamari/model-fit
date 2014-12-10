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
      -- * Helpers
    , liftOp
    ) where

import Data.Monoid
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M

import ModelFit.Model ( Packed, packed, FitDesc, Curve, fit, ParamLoc, FitExpr
                      , expr, hoist, liftOp, fitEval, evalParam)
import qualified ModelFit.Model as Mo
import ModelFit.Types

type Params p = M.Map String (Mo.FitExpr (Mo.ParamLoc p) p p)
type FitExprM p = Mo.FitExprT p (Writer (Params p))
type GlobalFitM p = Mo.GlobalFitT p (Writer (Params p)) 

globalParam :: String -> a -> GlobalFitM a (FitExprM a a)
globalParam name initial = do
    p <- Mo.globalParam initial >>= Mo.expr
    lift $ tell $ M.singleton name p
    --return $ WriterT $ fmap (\a->(a, mempty)) (Mo.hoist p)
    undefined

param :: String -> a -> FitExprM a a
param name initial = do
    p <- Mo.paramExpr initial
    lift $ tell $ M.singleton name p
    undefined

runGlobalFitM :: (Num p, VS.Storable p)
              => GlobalFitM p a
              -> (a, [Curve p], Packed VS.Vector p, Params p)
runGlobalFitM action =
    let ((a,b,c), d) = runWriter $ Mo.runGlobalFitT action
    in (a,b,c,d)
