{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (sequence, mapM, foldl)
import Data.Traversable
import Data.Foldable
import Control.Monad (void)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.Vector.Algorithms.Heap (sort)
import Options.Applicative

import Linear
import Control.Lens hiding (argument)

import Graphics.Rendering.Chart hiding (Point)
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default
import Data.Colour
import Data.Colour.Names

import ModelFit.Model.Named
import ModelFit.Types (Point, withVar)
import ModelFit.Fit
import ModelFit.FitExpr
import ModelFit.Models.Lifetime

import CsvUtils

data Opts = Opts { irfPath    :: FilePath
                 , fluorPath  :: [FilePath]
                 , components :: Int
                 }

opts :: Parser Opts
opts = Opts
    <$> strOption (long "irf" <> metavar "FILE" <> help "IRF curve path")
    <*> many (strArgument (metavar "FILE" <> help "Fluorescence curve path"))
    <*> option auto (long "components" <> short 'n' <> value 1 <> metavar "N" <> help "Number of components")

printing :: EitherT String IO () -> IO ()
printing action = runEitherT action >>= either print return

jiffy :: Double
jiffy = 8

dropLast n v = V.take (V.length v - n) v

mode :: (VG.Vector v a, Ord a, Eq a) => v a -> Maybe a
mode v | VG.null v = Nothing
mode v = go (VG.head v', 1) $ VG.tail v'
  where
    v' = sorted v
    sorted :: (VG.Vector v a, Ord a) => v a -> v a
    sorted v = VG.create $ do
        v' <- VG.thaw v
        sort v'
        return v'
    go (a,n) v
      | VG.null v  = Just a
      | n' > n     = go (a', n') y
      | otherwise  = go (a,  n)  y
      where
        (x,y) = VG.span (== a') v
        n' = VG.length x
        a' = VG.head v

sumModels :: (Applicative f, Num a) => [f (b -> a)] -> f (b -> a)
sumModels = foldl (\accum m->liftOp (+) <$> accum <*> m) (pure $ const 0)

main :: IO ()
main = printing $ do
    args <- liftIO $ execParser $ info (helper <*> opts) (fullDesc <> progDesc "Fit fluorescence decays")
    let withPoissonVar = withVar id
    irfPts <- V.take 4095 . V.map withPoissonVar <$> readPoints (irfPath args)
    fluorPts <- mapM (\fname->V.take 4095 . V.map withPoissonVar <$> readPoints fname) (fluorPath args)

    let Just irfBg = mode $ V.map (^. _y) $ V.take 1500 irfPts -- HACK
    let irfHist = V.convert $ V.map (subtract irfBg) $ V.drop offset $ V.map (^._y) irfPts
        offset = 0
        --period = round $ 1/80e6 / (jiffy * 1e-12)
        period = findPeriod irfHist
        periods = 2

    liftIO $ putStrLn $ "Period: "++show period
    liftIO $ putStrLn $ "IRF background: "++show irfBg
    let irf = mkIrf irfHist (periods*period)

    let fitFluor :: String                      -- ^ Curve name
                 -> [FitExprM Double Double]    -- ^ lifetimes
                 -> V.Vector (Point Double)     -- ^ points
                 -> GlobalFitM Double (FitDesc Double)
        fitFluor name taus pts = do
            let fluorBg = V.head pts ^. _y
                Just fluorAmp = maximumOf (each . _y) pts
                fitPts = V.convert $ V.take period pts
            let component :: Int -> FitExprM Double Double -> FitExprM Double (Double -> Double)
                component i tau = lifetimeModel <$> p
                  where p = sequenceA $ LifetimeP { _decayTime = tau
                                                  , _amplitude = param (name++"-amp"++show i) (fluorAmp / 2)
                                                  }
            decayModel <- expr $ sumModels $ zipWith component [1..] taus
            --background <- expr $ const <$> param "bg" fluorBg
            let background = return $ const 0
            convolved <- expr $ convolvedModel irf (periods*period) jiffy <$> hoist decayModel
            m <- expr $ liftOp (+) <$> hoist convolved <*> hoist background
            fit fitPts $ hoist m

    let (fds, curves, p0, params) = runGlobalFitM $ do
          taus <- mapM (\i->globalParam ("tau"++show i) (realToFrac $ i*1000)) [1..components args]
          forM (zip ["curve"++show i | i <- [1..]] fluorPts) $ \(name,pts) -> fitFluor name taus pts

    let Right fit = leastSquares curves p0

    liftIO $ print $ fmap (flip evalParam p0 . Param) params
    liftIO $ print $ fmap (flip evalParam fit . Param) params
    forM_ (zip3 (fluorPath args) fluorPts fds) $ \(fname,pts,fd) -> do
        let path = fname++".png"
            ts = take 3000 $ V.toList $ V.map (^._x) irfPts
        liftIO $ void $ renderableToFile def path
               $ toRenderable $ plotFit ts pts (fitEval fd fit)
        liftIO $ putStrLn $ "Reduced Chi squared: "++show (reducedChiSquared fd fit)

    return ()

plotFit :: forall a. (Show a, RealFloat a, PlotValue a)
        => [a] -> V.Vector (Point a) -> (a -> a) -> StackedLayouts a
plotFit ts pts fit =
    def & slayouts_layouts .~ [StackedLayout layout]
  where
    layout = def & layout_plots .~ plots
                 & layout_y_axis . laxis_generate .~ autoScaledLogAxis def

    plots :: [Plot a a]
    plots =
      [ toPlot $ def & plot_points_values .~ zip [realToFrac $ jiffy*i | i <- [0..]] (toListOf (each . _y) pts)
                     & plot_points_style . point_color .~ opaque green
                     & plot_points_title .~ "Observed"
      , toPlot $ def & plot_lines_values .~ [map (\x->(x, fit x)) ts]
                     & plot_lines_style . line_color .~ opaque red
                     & plot_lines_title .~ "Fit"
      ]
