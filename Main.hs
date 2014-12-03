import Data.Foldable as F
import Data.Functor.Identity
import qualified Data.Vector.Storable as V
import Control.Applicative
import Data.Functor.Product

import Linear
import Control.Lens

import Model       
import Fit
import Models.Fcs

liftOp :: (a -> a -> a) -> (b -> a) -> (b -> a) -> (b -> a)
liftOp op f g = \x -> f x `op` g x

sources pts = runGlobalFitM $ do
    diff1 <- globalParam 2.3 :: GlobalFitM Double (FitExprM Double Double)
    diff2 <- globalParam 3.2
    aspect <- globalParam 10
    a <- expr $ Diff3DP <$> diff1 <*> fixed 1 <*> aspect <*> param 1
    b <- expr $ Diff3DP <$> diff2 <*> fixed 1 <*> aspect <*> param 1
    fit pts $ liftOp (*) <$> fmap (model diff3DModel) (hoist a)
                         <*> fmap (model diff3DModel) (hoist b)
    return (a, b)

main = do
    let genParams1 = defaultParams & (diffTime      .~ 10)
                                   . (concentration .~ 2)
        genParams2 = defaultParams & (diffTime      .~ 30)
                                   . (concentration .~ 2)
        genParams = Pair genParams1 genParams2
        m = opModel (*) diff3DModel diff3DModel
        (curves, p0, (unpackA, unpackB)) = sources points
        points = V.fromList [ let x = 2**i in Point x (model m genParams x) 1
                            | i <- [1, 1.1..10]
                            ]

    let Right fit = leastSquares curves p0
    print genParams
    print $ evalParam unpackA p0
    print $ evalParam unpackB fit
