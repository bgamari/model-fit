{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Functor.Identity
import Control.Applicative
import Control.Monad       
import Control.Monad.IO.Class
import Control.Lens
import Control.Error
import Data.Foldable as F
import Data.Char (ord)

import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.IntMap as IM
import qualified Data.Vector as V
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

instance FromRecord (Point V1 V1 Double) where
    parseRecord v
      | V.length v == 3 = Point <$> v Csv..! 0
                                <*> v Csv..! 1
                                <*> v Csv..! 2
      | otherwise       = mzero

readPoints :: BSL.ByteString -> Either String (V.Vector (Point V1 V1 Double))
readPoints = Csv.decodeWith decOpts True
  where decOpts = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }
 
data Curve = Curve { _cPoints :: V.Vector (Point V1 V1 Double)
                   , _cName   :: String
                   }
makeLenses ''Curve    

data Session param = Session
    { _sCurves :: V.Vector Curve
    , _sModel  :: Model param V1 V1
    , _sParams :: param Double
    }
makeLenses ''Session    

modelPlot :: (Functor curves, Foldable curves)
          => curves Curve
          -> Model param V1 V1
          -> curves (param Double)
          -> [Double]
          -> Layout1 Double Double
modelPlot curves m params xs =
    def & layout1_plots .~ ( map (Left . toPlot) curvePlots
                          ++ map (Left . toPlot) modelPlots)
  where
    curvePlots = F.toList $ fmap plotCurve curves
    modelPlots = F.toList $ fmap (\p->plotModel m p xs) params

plotCurve :: Curve -> PlotErrBars Double Double
plotCurve c = def & (plot_errbars_title  .~ (c ^. cName))
                  . (plot_errbars_values .~ values)
  where values :: [ErrPoint Double Double]
        values = map toErrPoint $ F.toList (c ^. cPoints)

plotModel :: Model param V1 V1
          -> param Double
          -> [Double]
          -> PlotLines Double Double
plotModel m p xs =
    def & plot_lines_values .~ [map (\x->(x, model m p (V1 x) ^. _x)) xs]

toErrPoint :: Num a => Point V1 V1 a -> ErrPoint a a
toErrPoint (Point (V1 x) (V1 y) (V1 e)) =
    ErrPoint (ErrValue x x x) (ErrValue (y-e) y (y+e))

path = "/home/ben/lori/data/sheema/2013-07-16/2013-07-16-run_001.timetag.acorr-0"
main = main' >>= print
main' = runEitherT $ do
    points' <- liftIO (BSL.readFile path) >>= EitherT . return . readPoints
    let points = V.filter (views ptX (>1))
               $ points' & (mapped . ptY . mapped %~ subtract 1)
                         . (mapped . ptX . mapped %~ (*1e6))
    liftIO $ F.mapM_ print points
    let packedParams = PP $ IM.fromList $ zip [0..] [70, 0.8, 0.08]
        sources = Diff3DP { _diffTime      = FromVector $ PIdx 0
                          , _diffExponent  = Fixed 1
                          , _aspectRatio   = FromVector $ PIdx 1
                          , _concentration = FromVector $ PIdx 2
                          }
        m = diff3DModel
        fits = takeEvery 200 $ fit m (Identity points) (Identity sources) packedParams

    let unpck = runIdentity $ unpack (Identity sources) packedParams
        xs = [10**i | i <- [0, 0.01 .. 4]]
    liftIO $ renderableToSVGFile
        ( toRenderable
        $ layout1_plots .~ [ Left $ toPlot $ plotCurve $ Curve points "hi"
                           , Left $ toPlot $ plotModel m unpck xs]
        $ layout1_bottom_axis . laxis_generate .~ autoScaledLogAxis def
        $ def
        ) 640 480 "hi.svg"

    liftIO $ F.forM_ fits $ \p->do
        print (chiSquared m p (Identity points), runIdentity p)
    return ()
