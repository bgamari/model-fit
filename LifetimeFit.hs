import Prelude hiding (sequence, mapM, foldl)
import Data.Traversable
import Data.Foldable
import Control.Monad (void)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Options.Applicative
import Data.Functor.Product
import Data.Functor.Identity

import qualified Data.Map as M
import Control.Monad.Writer (WriterT, runWriterT, tell)

import Linear
import Control.Lens hiding (argument)

import Graphics.Rendering.Chart hiding (Point)
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default
import Data.Colour
import Data.Colour.Names

import ModelFit.Model.Named
import ModelFit.Types (withVar)
import ModelFit.Fit
import ModelFit.Models.Lifetime

import CsvUtils

data Opts = Opts { irfPath    :: FilePath
                 , fluorPath  :: FilePath
                 , components :: Int
                 }

opts = Opts
    <$> strOption (long "irf" <> metavar "FILE" <> help "IRF curve path")
    <*> strArgument (metavar "FILE" <> help "Fluorescence curve path")
    <*> option auto (long "irf" <> metavar "N" <> help "Number of components")

printing :: EitherT String IO () -> IO ()
printing action = runEitherT action >>= either print return

jiffy = 8 :: Double

dropLast n v = V.take (V.length v - n) v

type NamedGlobalFitM s = WriterT (M.Map String (FitExpr (ParamLoc s) s s)) (GlobalFitM s)

namedGlobalParam :: String -> a -> NamedGlobalFitM a (FitExprM a a)
namedGlobalParam name initial = do
    p <- lift $ globalParam initial >>= expr
    tell $ M.singleton name p
    return $ hoist p

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
    let (curves, p0, params, fd) = runGlobalFitM $ do
          taus <- mapM (\i->globalParam ("tau"++show i) (realToFrac $ i*1000)) [1..components args]
          let component :: FitExprM Double Double -> FitExprM Double (Double -> Double)
              component tau = lifetimeModel <$> p
                where p = sequenceA $ LifetimeP { _decayTime = tau
                                                , _amplitude = param $ fluorAmp / 2
                                                }
          decayModel <- lift $ expr $ foldl (\accum tau->liftOp (+) <$> accum <*> component tau) (pure $ const 0) taus
          --background <- lift $ expr $ (\p _ -> p) <$> param fluorBg
          let background = return $ const 0
          convolved <- lift $ expr $ convolvedModel irf (periods*period) jiffy <$> hoist decayModel
          m <- lift $ expr $ liftOp (+) <$> hoist convolved <*> hoist background
          --let m = convolved
          lift $ fit fitPts $ hoist m

    let Right fit = leastSquares curves p0
    --let fit = p0

    liftIO $ plot "hi.png" [ let f = fitEval fd fit
                                 ts = take 3000 $ V.toList $ V.map (^._x) irfPts
                             in map (\t->(t, f t)) ts
                           , zip [jiffy*i | i <- [0..]] (toListOf (each . _y) fluorPts)
                           ]
    liftIO $ print $ fmap (flip evalParam fit) params
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
