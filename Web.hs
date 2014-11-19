{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (sequence)
import Data.Functor.Identity
import Control.Applicative
import Control.Monad (mzero)
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
import Data.Csv as Csv

import Control.Lens       
import Linear
import Graphics.Rendering.Chart hiding (Point)
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default

import FcsFit hiding (main)
import Model
import FcsModels

instance FromField a => FromField (V1 a) where
    parseField f = V1 <$> parseField f

instance FromRecord (Point Double) where
    parseRecord v
      | V.length v == 3 = Point <$> v Csv..! 0
                                <*> v Csv..! 1
                                <*> v Csv..! 2
      | otherwise       = mzero

readPoints :: BSL.ByteString -> Either String (V.Vector (Point Double))
readPoints = Csv.decodeWith decOpts HasHeader
  where decOpts = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
 
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
    def & plot_lines_values .~ [map (\x->(x, model m p x)) xs]

toErrPoint :: Num a => Point a -> ErrPoint a a
toErrPoint (Point x y e) =
    ErrPoint (ErrValue x x x) (ErrValue (y-e) y (y+e))

--path = "/home/ben/lori/data/sheema/2013-07-16/2013-07-16-run_001.timetag.acorr-0"
path = "hello"

main = main' >>= print
main' = runEitherT $ do
    points' <- liftIO (BSL.readFile path) >>= EitherT . return . readPoints
    let points = VS.filter (views ptX (>1))
               $ points' & mapped . ptY %~ subtract 1
                         & mapped . ptX %~ (*1e6)
                         & VS.convert

    let (packing, p0) = runParamsM $ sequence $
            Diff3DP { _diffTime      = param 10
                    , _diffExponent  = fixed 1
                    , _aspectRatio   = param 10
                    , _concentration = param 1
                    }
        m = diff3DModel
        Right fit = leastSquares packing [(points, m)] p0

    let xs = [10**i | i <- [0, 0.01 .. 4]]
    liftIO $ renderableToFile def "hi.png"
           $ toRenderable
           $ layout_plots .~ [ toPlot $ plotCurve $ Curve points "hi"
                             , toPlot $ plotModel m (unpack packing fit) xs]
       --  $ layout_bottom_axis . laxis_generate .~ autoScaledLogAxis def
           $ def

    liftIO $ print (chiSquared (VS.toList points) m (unpack packing fit), unpack packing fit)
    return ()
