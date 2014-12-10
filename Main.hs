import Data.Foldable as F
import Data.Functor.Identity
import qualified Data.Vector.Storable as V
import Control.Applicative
import Data.Functor.Product

import Linear
import Control.Lens

import ModelFit.Types
import ModelFit.Fit
import ModelFit.Model
import ModelFit.Models.Fcs

sources pts = runGlobalFitM $ do
    diff1 <- globalParam 2.3 :: GlobalFitM Double (FitExprM Double Double)
    diff2 <- globalParam 3.2
    aspect <- globalParam 10
    a <- expr $ Diff3DP <$> diff1 <*> fixed 1 <*> aspect <*> param 1
    b <- expr $ Diff3DP <$> diff2 <*> fixed 1 <*> aspect <*> param 1
    fit pts $ liftOp (*) <$> fmap diff3DModel (hoist a)
                         <*> fmap diff3DModel (hoist b)
    return (a, b)

main = do
    let genParams1 = defaultParams & (diffTime      .~ 10)
                                   . (concentration .~ 2)
        genParams2 = defaultParams & (diffTime      .~ 30)
                                   . (concentration .~ 2)
        m = liftOp (*) (diff3DModel genParams1) (diff3DModel genParams2)
        ((unpackA, unpackB), curves, p0) = sources points
        points = V.fromList [ let x = 2**i in Point x (m x) 1
                            | i <- [1, 1.1..10]
                            ]

    let Right fit = leastSquares curves p0
    print $ evalParam unpackA p0
    print $ evalParam unpackB fit
