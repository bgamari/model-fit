import Prelude hiding (sequence)
import Data.Traversable
import Control.Monad (void)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Options.Applicative
import Data.Functor.Product
import Data.Functor.Identity

import Linear
import Control.Lens hiding (argument)

import Graphics.Rendering.Chart hiding (Point)
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default
import Data.Colour
import Data.Colour.Names

import Types (withVar)
import CsvUtils
import Model
import Fit
import Models.Lifetime

data Opts = Opts { irfPath   :: FilePath
                 , fluorPath :: FilePath
                 }

opts = Opts
    <$> strOption (long "irf" <> metavar "FILE" <> help "IRF curve path")
    <*> strArgument (metavar "FILE" <> help "Fluorescence curve path")

printing :: EitherT String IO () -> IO ()
printing action = runEitherT action >>= either print return

jiffy = 8 :: Double

dropLast n v = V.take (V.length v - n) v

main = printing $ do
    args <- liftIO $ execParser $ info (helper <*> opts) (fullDesc <> progDesc "Fit fluorescence decays")
    let withPoissonVar = withVar id
    irfPts <- V.take 4095 . V.map withPoissonVar <$> readPoints (irfPath args)
    fluorPts <- V.take 4095 . V.map withPoissonVar <$> readPoints (fluorPath args)

    let irfHist = V.convert $ V.map (subtract 30) $ V.map (^._y) irfPts
        --period = round $ 1/80e6 / (jiffy * 1e-12)
        period = findPeriod irfHist
        periods = 2
        irf = mkIrf irfHist (periods*period)

    let fluorBg = V.head fluorPts ^. _y
        Just fluorAmp = maximumOf (each . _y) fluorPts
        fitPts = V.convert $ V.take period fluorPts
    let (curves, p0, fd) = runGlobalFitM $ do
          let component :: Double -> FitExprM Double (Double -> Double)
              component tau = lifetimeModel <$> p
                where p = sequenceA $ LifetimeP { _decayTime = param tau
                                                , _amplitude = param $ fluorAmp / 2
                                                }
          --decayModel <- expr $ liftOp (+) <$> component 2000 <*> component 4000
          decayModel <- expr $ component 2000
          background <- expr $ (\p _ -> p) <$> param fluorBg
          convolved <- expr $ convolvedModel irf (periods*period) jiffy <$> hoist decayModel
          m <- expr $ liftOp (+) <$> hoist convolved <*> hoist background
          --let m = convolved
          fit fitPts $ hoist m

    let Right fit = leastSquares curves p0
    --let fit = p0

    liftIO $ plot "hi.png" [ let f = fitEval fd fit
                                 ts = take 3000 $ V.toList $ V.map (^._x) irfPts
                             in map (\t->(t, f t)) ts
                           , zip [jiffy*i | i <- [0..]] (toListOf (each . _y) fluorPts)
                           ]
    --liftIO $ print $ unpack packing p0
    liftIO $ print period
    liftIO $ putStrLn $ "Reduced Chi squared: "++show (reducedChiSquared fd fit)

    return ()

plot :: FilePath -> [[(Double, Double)]] -> IO ()
plot path curves =
    void $ renderableToFile def path
         $ toRenderable
         $ layout_plots .~
             zipWith (\pts c->toPlot $ def & plot_points_values .~ pts
                                           & plot_points_style . point_color .~ c) curves (cycle colors)
         $ layout_y_axis . laxis_generate .~ autoScaledLogAxis def
         $ def

colors :: [AlphaColour Double]
colors = map opaque [red, green, blue, yellow, orange, purple]
