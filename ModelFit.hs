{-# LANGUAGE RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import           Control.Applicative
import qualified Data.ByteString.Lazy as BS
import           Data.Char
import           Data.Csv
import           Data.Foldable as Foldable
import           Data.Traversable
import qualified Data.Vector as V
import           Numeric.AD
import           Numeric.AD.Internal.Classes (Lifted)
import           Numeric.AD.Types (AD)
import           Optimization.LineSearch.ConjugateGradient

import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Graphics.Rendering.Chart
import           System.Environment

data FcsModel a = Fcs { fcsTauD     :: !a  -- ^ diffusion time (us)
                      , fcsA        :: !a  -- ^ aspect ratio
                      , fcsG0       :: !a  -- ^ amplitude
                      , fcsGinf     :: !a  -- ^ offset
                      , fcsAlpha    :: !a  -- ^ anomalous diffusion exponent
                      }
                deriving (Show, Eq, Functor, Foldable, Traversable)

data Scope a = Fixed a
             | Fitted
             deriving (Show)

withScope :: Applicative f => f (Scope a) -> f a -> f a
withScope s x = f <$> s <*> x
  where f :: Scope a -> a -> a
        f (Fixed x) _ = x
        f Fitted    x = x

fcsCorr :: (RealFloat a) => FcsModel a -> a -> a
fcsCorr (Fcs {..}) tau =
    fcsG0 / (1 + ttd) / sqrt (1 + fcsA**(-2) * ttd) + fcsGinf
  where ttd = tau / fcsTauD

sumSqResidual :: (Functor f, Foldable f, RealFrac a) => (a -> a) -> f (Obs a) -> a
sumSqResidual f = Foldable.sum . fmap (\(Obs x y s)->(f x - y)^2 / s)

main = do
    fname:_ <- getArgs
    Right corr' <- readCorr fname
    let corr = V.map (\o->o {oX=1e6*oX o}) $ V.filter (\o->oX o > 1e-6) corr'
    let p0 = Fcs { fcsTauD  = 100
                 , fcsA     = 10
                 , fcsG0    = 10
                 , fcsGinf  = 1
                 , fcsAlpha = 1
                 }
    let fit :: Lifted s => FcsModel (AD s Double) -> AD s Double
        fit p = sumSqResidual (fcsCorr p) $ fmap (fmap realToFrac) corr
    print p0
    let go :: (a -> String) -> Int -> [a] -> IO a
        go show 0 (x:_)     = return x
        go show _ [x]       = return x
        go show n (x:rest)  = putStrLn (show x) >> go show (n-1) rest
    --let p1 = head $ drop 100 $ conjugateGradientDescent fit p0
    p1 <- go (\p->show (sumSqResidual (fcsCorr (fmap realToFrac p)) corr, p)) 100 $ conjugateGradientDescent fit p0
    print p1
    renderableToSVGFile (toRenderable $ plotFit corr [fcsCorr p1])
                        800 800 "out.svg"

data Obs a = Obs { oX :: !a   -- ^ Abscissa
                 , oY :: !a   -- ^ Ordinate
                 , oE :: !a   -- ^ Variance
                 }
             deriving (Show, Eq, Functor, Foldable, Traversable)

instance FromField a => FromRecord (Obs a) where
    parseRecord v
        | V.length v == 3  = Obs <$> v .! 0
                                 <*> v .! 1
                                 <*> v .! 2

readCorr :: FilePath -> IO (Either String (V.Vector (Obs Double)))
readCorr fname =
    fmap (decodeWith opts False) $ BS.readFile fname
  where opts = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }

plotFit :: (RealFloat a, Enum a, PlotValue a, Show a)
        => V.Vector (Obs a) -> [a -> a] -> Layout1 a a
plotFit pts fits = layout
  where layout = layout1_plots .~ ([Left points]++map (Left . fit) fits)
                 $ layout1_bottom_axis . laxis_generate .~ (autoScaledLogAxis defaultLogAxis)
                 $ defaultLayout1
        points = toPlot
                 $ plot_points_style .~ filledCircles 1 (opaque green)
                 $ plot_points_values .~ (V.toList $ V.map (\(Obs x y _)->(x,y)) pts)
                 $ plot_points_title .~ "Points"
                 $ defaultPlotPoints
        fit f  = toPlot
                 $ plot_lines_values .~ [map (\x->(x, f x)) xs]
                 $ defaultPlotLines
        xs = let a = V.minimum $ V.map oX pts
                 b = V.maximum $ V.map oX pts
             in logSpace a b 1000

logSpace :: (RealFloat a, Enum a) => a -> a -> Int -> [a]
logSpace a b n = map exp $ [la,la+dx..lb]
  where la = log a
        lb = log b
        dx = (lb - la) / fromIntegral n
