{-# LANGUAGE RecordWildCards, MultiParamTypeClasses,
    DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric,
    FlexibleInstances, FlexibleContexts, RankNTypes,
    KindSignatures, DataKinds, TypeOperators #-}

import           Control.Applicative
import qualified Data.ByteString.Lazy as BS
import           Data.Char
import           Data.Csv
import           Data.Foldable as Foldable
import           Data.Traversable
import qualified Data.Vector as V

import           Numeric.AD
import           Numeric.AD.Internal.Classes (Lifted)
import           Numeric.AD.Types
import           Linear
import           Optimization.LineSearch.ConjugateGradient
import           Optimization.Constrained.Penalty
import           FcsModels

import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Graphics.Rendering.Chart
import           System.Environment

sumSqResidual :: (Functor f, Foldable f, RealFrac a) => (a -> a) -> f (Obs a) -> a
sumSqResidual f = Foldable.sum . fmap (\(Obs x y s)->(f x - y)^2 / s)

data Obs a = Obs { oX :: !a   -- ^ Abscissa
                 , oY :: !a   -- ^ Ordinate
                 , oE :: !a   -- ^ Variance
                 }
             deriving (Show, Eq, Functor, Foldable, Traversable)

fitModel :: RealFrac a => SumM FcsTriplet FcsModel a
fitModel = SumM t0 p1
  where p0 = Fcs { fcsTauD  = 100
                 , fcsA     = 10
                 , fcsG0    = 10
                 , fcsGinf  = 1
                 , fcsAlpha = 1
                 }
        t0 = FcsT { fcsF    = 0.1
                  , fcsTauF = 0.1
                  , fcsFcs = p0
                  }
        p1 = Fcs { fcsTauD  = 200
                 , fcsA     = 10
                 , fcsG0    = 10
                 , fcsGinf  = 1
                 , fcsAlpha = 1
                 }


opt :: (Mode s, Model f (AD s a)) => V.Vector (Obs a) -> Opt f a
opt obs = optimize (fit obs)

fit :: (Mode s, Model f (AD s a), RealFrac a)
    => V.Vector (Obs a) -> f (AD s a) -> AD s a
fit obs p = sumSqResidual (model p) $ fmap (fmap realToFrac) obs

main = do
    fname:_ <- getArgs
    Right corr' <- readCorr fname
    let corr = V.map (\o->o {oX=1e6*oX o})
               -- $ V.filter (\o->oX o > 1e-6)
               corr'

    let go :: (a -> String) -> Int -> [a] -> IO a
        go show 0 (x:_)     = return x
        go show _ [x]       = return x
        go show n (x:rest)  = putStrLn (show x) >> go show (n-1) rest

    let l0 = V.replicate 1 1

    let search = backtrackingSearch 0.1 0.2
        beta = fletcherReeves
        xmin (FU f) = conjGrad search beta (lowerFU f) (grad f)
        opt' = opt corr :: Opt (SumM FcsTriplet FcsModel) Double
    p1 <- go (\p->show (fit corr p, p)) 1000
             $ takeUntilConverged (absDeltaConv (fit corr) 0.1)
             $ minimize xmin opt' 2 fitModel l0
    print p1
    --renderableToSVGFile (toRenderable $ plotFit corr [model p1]) 800 800 "out.svg"

instance FromField a => FromRecord (Obs a) where
    parseRecord v
        | V.length v == 3  = Obs <$> v .! 0
                                 <*> v .! 1
                                 <*> v .! 2

-- | Take iterates until the given function of the last pair of iterates
-- returns @True@
takeUntilConverged :: (a -> a -> Bool) -> [a] -> [a]
takeUntilConverged f xs = go xs
  where go (x:[]) = [x]
        go (x:y:xs) | f y x      = x:y:[]
                    | otherwise  = x:go (y:xs)

absDeltaConv :: (RealFrac r) => (a -> r) -> r -> a -> a -> Bool
absDeltaConv f tol a b = abs (f a - f b) < tol

relDeltaConv :: (RealFrac r) => (a -> r) -> r -> a -> a -> Bool
relDeltaConv f tol a b = abs (f a - f b) / f a < tol

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
