import Prelude hiding (sequence)
import Data.Traversable
import Control.Monad (void)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import qualified Data.Vector as V
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

main = printing $ do
    args <- liftIO $ execParser $ info (helper <*> opts) (fullDesc <> progDesc "Fit fluorescence decays")
    let withPoissonVar = withVar id
    irfPts <- V.map withPoissonVar <$> readPoints (irfPath args)
    fluorPts <- V.map withPoissonVar <$> readPoints (fluorPath args)

    let period = round $ 1/80e6 / (jiffy * 1e-12)
        irf = mkIrf (V.convert $ V.map (subtract 30) $ V.map (^._y) irfPts) (2*period)

    let decayModel = sumModel lifetimeModel lifetimeModel
        --decayModel = lifetimeModel
        m = sumModel (convolvedModel irf (2*period) jiffy decayModel) constModel
        --m = convolvedModel irf (2*period) jiffy decayModel

    let fluorBg = V.head fluorPts ^. _y
    let ts = take 2000 $ V.toList $ V.map (^._x) irfPts
        params = LifetimeP 1000 11
        (packing, p0) = runParamsM $ do
            a <- sequence $ LifetimeP { _decayTime = param 2000
                                      , _amplitude = param 10000
                                      }
            b <- sequence $ LifetimeP { _decayTime = param 4000
                                      , _amplitude = param 10000
                                      }
            c <- Identity <$> param fluorBg
            return $ Pair (Pair a b) c
            --return $ Pair a c
            --return b

    let Right fit = leastSquares packing [(V.convert $ V.take 2000 $ fluorPts, m)] p0
    --let fit = p0

    liftIO $ plot "hi.png" [ let f = model m (unpack packing fit)
                             in map (\t->(t, f t)) ts
                           , zip [jiffy*i | i <- [0..]] (toListOf (each . _y) fluorPts)
                           ]
    let unpacked = unpack packing fit
    liftIO $ print unpacked
    liftIO $ putStrLn $ "Reduced Chi squared: "++show (reducedChiSquared (V.convert $ V.take 2000 fluorPts) m unpacked)

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
