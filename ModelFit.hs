{-# LANGUAGE RecordWildCards, MultiParamTypeClasses,
    DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric,
    FlexibleInstances, FlexibleContexts,
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
import           Linear.V

import           Optimization.LineSearch.ConjugateGradient
import Constrained

import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Graphics.Rendering.Chart
import           System.Environment

class Model f a where
    model :: f a -> a -> a

-- | Kinetic model
data Kinetic a = KineticM { kG0   :: a
                          , kTauB :: a
                          }
                   deriving (Show, Eq, Functor, Foldable, Traversable)

instance Additive Kinetic where zero = pure 0
instance Metric Kinetic
instance Applicative Kinetic where
    pure a = KineticM a a
    KineticM a b <*> KineticM a' b' = KineticM (a a') (b b')

instance RealFloat a => Model Kinetic a where
    model (KineticM {..}) tau = kG0 * exp(-tau / kTauB)

-- | Diffusion model
data FcsModel a = Fcs { fcsTauD     :: a  -- ^ diffusion time (us)
                      , fcsA        :: a  -- ^ aspect ratio
                      , fcsG0       :: a  -- ^ amplitude
                      , fcsGinf     :: a  -- ^ offset
                      , fcsAlpha    :: a  -- ^ anomalous diffusion exponent
                      }
                deriving (Show, Eq, Functor, Foldable, Traversable)

instance Additive FcsModel where zero = pure 0
instance Metric FcsModel
instance Applicative FcsModel where
    pure a = Fcs a a a a a
    Fcs a b c d e <*> Fcs a' b' c' d' e' = Fcs (a a') (b b') (c c') (d d') (e e')

instance RealFloat a => Model FcsModel a where model = fcsCorr

fcsCorr :: (RealFloat a) => FcsModel a -> a -> a
fcsCorr (Fcs {..}) tau =
    fcsG0 / (1 + ttd) / sqrt (1 + 1/(fcsA*fcsA) * ttd) + fcsGinf
  where ttd = tau / fcsTauD

-- | Diffusion model with triplet
data FcsTriplet a = FcsT { fcsF    :: a
                         , fcsTauF :: a
                         , fcsFcs  :: FcsModel a
                         }
                  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Metric FcsTriplet
instance Additive FcsTriplet where zero = pure 0
instance Applicative FcsTriplet where
    pure a = FcsT a a (pure a)
    FcsT a b c <*> FcsT a' b' c'  = FcsT (a a') (b b') (c <*> c')

instance RealFloat a => Model FcsTriplet a where
    model = fcsTripletCorr

fcsTripletCorr :: (RealFloat a) => FcsTriplet a -> a -> a
fcsTripletCorr (FcsT {..}) tau =
    (1 - fcsF + fcsF * exp (-tau / fcsTauF)) / (1 - fcsF) * fcsCorr fcsFcs tau


-- | Sum model
data SumM f g a = SumM (f a) (g a)
                deriving (Show, Eq, Functor, Foldable, Traversable)

instance (Additive f, Applicative f, Additive g, Applicative g) => Additive (SumM f g) where zero = pure 0
instance (Metric f, Foldable f, Applicative f, Metric g, Foldable g, Applicative g) => Metric (SumM f g)
instance (Applicative f, Applicative g) => Applicative (SumM f g) where
    pure a = SumM (pure a) (pure a)
    SumM f g <*> SumM f' g' = SumM (f <*> f') (g <*> g')

instance (Model f a, Model g a, Num a) => Model (SumM f g) a where
    model (SumM fa ga) a = model fa a + model ga a

-- | Product model
data ProdM f g a = ProdM (f a) (g a)
                deriving (Show, Eq, Functor, Foldable, Traversable)

instance (Applicative f, Applicative g) => Applicative (ProdM f g) where
    pure a = ProdM (pure a) (pure a)
    ProdM f g <*> ProdM f' g' = ProdM (f <*> f') (g <*> g')

instance (Additive f, Applicative f, Additive g, Applicative g) => Additive (ProdM f g) where
    zero = pure 0

instance (Metric f, Foldable f, Applicative f, Metric g, Foldable g, Applicative g) =>
         Metric (ProdM f g)

instance (Model f a, Model g a, Num a) => Model (ProdM f g) a where
    model (ProdM fa ga) a = model fa a * model ga a


sumSqResidual :: (Functor f, Foldable f, RealFrac a) => (a -> a) -> f (Obs a) -> a
sumSqResidual f = Foldable.sum . fmap (\(Obs x y s)->(f x - y)^2 / s)

data Obs a = Obs { oX :: !a   -- ^ Abscissa
                 , oY :: !a   -- ^ Ordinate
                 , oE :: !a   -- ^ Variance
                 }
             deriving (Show, Eq, Functor, Foldable, Traversable)

main = do
    fname:_ <- getArgs
    Right corr' <- readCorr fname
    let corr = V.map (\o->o {oX=1e6*oX o})
               -- $ V.filter (\o->oX o > 1e-6)
               corr'
    let p0 = Fcs { fcsTauD  = 100
                 , fcsA     = 10
                 , fcsG0    = 10
                 , fcsGinf  = 1
                 , fcsAlpha = 1
                 }
        t0 = FcsT { fcsF    = 0.1
                  , fcsTauF = 0.1
                  , fcsFcs = p0
                  }
    --let fit :: RealFloat a => FcsModel a -> a
    --    fit p = sumSqResidual (fcsCorr p) $ fmap (fmap realToFrac) corr
    --    dfit :: RealFloat a => FcsModel a -> FcsModel a
    --    dfit = grad fit
    let fit :: RealFloat a => FcsTriplet a -> a
        fit p = sumSqResidual (fcsTripletCorr p) $ fmap (fmap realToFrac) corr
        opt = constrainEQ (\fcs -> fcsTauF fcs - 0.2)
              $ optimize fit

    let go :: (a -> String) -> Int -> [a] -> IO a
        go show 0 (x:_)     = return x
        go show _ [x]       = return x
        go show n (x:rest)  = putStrLn (show x) >> go show (n-1) rest

    let l0 = V.replicate 1 1

    let search = backtrackingSearch 0.1 0.2
        beta = fletcherReeves
        xmin (FU f) = conjGrad search beta (lowerFU f) (grad f)
    p1 <- go (\p->show (sumSqResidual (fcsTripletCorr (fmap realToFrac p)) corr, p)) 1000
             $ minimize xmin opt 2 t0 l0
    print p1
    renderableToSVGFile (toRenderable $ plotFit corr [fcsTripletCorr p1]) 800 800 "out.svg"

instance FromField a => FromRecord (Obs a) where
    parseRecord v
        | V.length v == 3  = Obs <$> v .! 0
                                 <*> v .! 1
                                 <*> v .! 2

takeUntilConverged :: (a -> a -> Bool) -> [a] -> [a]
takeUntilConverged f xs = go xs
  where go (x:[]) = [x]
        go (x:y:xs) | f y x      = x:y:[]
                    | otherwise  = x:go (y:xs)

relErrorConverged :: (RealFrac r) => (a -> r) -> r -> a -> a -> Bool
relErrorConverged f err a b = let rel = (f a - f b) / f a in rel < err

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
