{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

import Prelude hiding (sequence)
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.IO.Class
import Control.Lens
import Control.Error
import Data.Foldable as F
import Data.Traversable
import Data.Char (ord)

import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString.Lazy as BSL

import Control.Lens
import Linear
import Graphics.Rendering.Chart hiding (Point)
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default

import CsvUtils
import ModelFit.Fit
import ModelFit.Model hiding (Curve)
import ModelFit.Models.Fcs hiding (defaultParams)
import ModelFit.Models.Lifetime

data Curve = Curve { _cPoints :: VS.Vector (Point Double)
                   , _cName   :: String
                   }
makeLenses ''Curve

data Session param = Session
    { _sCurves :: V.Vector Curve
    , _sModel  :: Model param Double
    , _sParams :: param Double
    }
makeLenses ''Session

modelPlot :: (a ~ Double, Functor curves, Foldable curves)
          => curves Curve
          -> Model param a
          -> curves (param a)
          -> [a]
          -> Layout a a
modelPlot curves m params xs =
    def & layout_plots .~ ( map toPlot curvePlots
                         ++ map toPlot modelPlots)
  where
    curvePlots = F.toList $ fmap plotCurve curves
    modelPlots = F.toList $ fmap (\p->plotModel m p xs) params

plotCurve :: Curve -> PlotErrBars Double Double
plotCurve c = def & plot_errbars_title  .~ view cName c
                  & plot_errbars_values .~ values
  where values :: [ErrPoint Double Double]
        values = map toErrPoint $ VS.toList (c ^. cPoints)

plotModel :: RealFloat a
          => Model param a
          -> param a
          -> [a]
          -> PlotLines a a
plotModel m p xs =
    def & plot_lines_values .~ [map (\x->(x, m p x)) xs]

toErrPoint :: Num a => Point a -> ErrPoint a a
toErrPoint (Point x y e) =
    ErrPoint (ErrValue x x x) (ErrValue (y-e) y (y+e))

data FitConfig = forall param. FitConfig
    { setupLayout   :: Layout Double Double -> Layout Double Double
    , prepareObs    :: V.Vector (Point Double) -> V.Vector (Point Double)
    , buildModel    :: VS.Vector (Point Double) -> GlobalFitM Double (param, FitDesc Double)
    , showParams    :: Packed VS.Vector Double -> param -> String
    }

fcs :: FitConfig
fcs = FitConfig
    { setupLayout = (layout_x_axis . laxis_generate .~ autoScaledLogAxis def)
                  . (layout_x_axis . laxis_title .~ "tau (seconds)")
                  . (layout_y_axis . laxis_title .~ "G(tau)")
    , prepareObs = \v-> v & mapped . ptY %~ subtract 1
                          & mapped . ptX %~ (*1e6)
                          & V.filter (views ptX (>1))
    , buildModel = \pts->do
          p <- expr $ sequenceA
               $ Diff3DP { _diffTime      = param 10
                         , _diffExponent  = fixed 1
                         , _aspectRatio   = param 10
                         , _concentration = param 1
                         }
          fd <- fit pts $ diff3DModel <$> hoist p
          return (p, fd)
    , showParams = \packed p -> show $ evalParam p packed
    }

lifetime :: FitConfig
lifetime = FitConfig
    { setupLayout = (layout_y_axis . laxis_generate .~ autoScaledLogAxis def)
                  . (layout_x_axis . laxis_title .~ "tau (seconds)")
                  . (layout_y_axis . laxis_title .~ "counts")
    , prepareObs = id
    , buildModel = \pts -> do
          p <- expr $ sequenceA
               $ LifetimeP { _decayTime = param 10, _amplitude = param 1 }
          fd <- fit pts $ lifetimeModel <$> hoist p
          return (p, fd)
    , showParams = \packed p -> show $ evalParam p packed
    }

--path = "/home/ben/lori/data/sheema/2013-07-16/2013-07-16-run_001.timetag.acorr-0"
path = "hello"

main = main' >>= print
main' = runEitherT $ do
    let mc = fcs
    FitConfig { buildModel=fm, showParams=showParams} <- return mc
    points' <- readPoints path
    let points = VS.convert $ prepareObs mc points'

    let (curves, p0, (params, fd)) = runGlobalFitM (fm points)
        Right fit = leastSquares curves p0

    let xs = [10**i | i <- [0, 0.01 .. 6]]
    liftIO $ renderableToFile def "hi.png"
           $ toRenderable
           $ layout_plots .~ [ toPlot $ plotCurve $ Curve points "hi" ]
                             -- , toPlot $ plotModel fm (evalParam params fit) xs]
           $ setupLayout mc
           $ def

    --liftIO $ print (evalParam params fit)
    liftIO $ print (chiSquared fd fit)
    liftIO $ print (reducedChiSquared fd fit)
    return ()
