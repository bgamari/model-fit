import Data.Foldable as F
import Data.Functor.Identity
import qualified Data.Vector.Storable as V
import Control.Applicative
import Data.Functor.Product

import Linear
import Control.Lens

import Model       
import FcsModels
import FcsFit

sources = runParamsM $ do
    diff1 <- param 2.3
    diff2 <- param 3.2
    aspect <- param 10
    a <- Diff3DP <$> pure diff1 <*> fixed 1 <*> pure aspect <*> param 1
    b <- Diff3DP <$> pure diff2 <*> fixed 1 <*> pure aspect <*> param 1
    return $ Pair a b

main = do
    let genParams1 = defaultParams & (diffTime      .~ 10)
                                   . (concentration .~ 2)
        genParams2 = defaultParams & (diffTime      .~ 30)
                                   . (concentration .~ 2)
        genParams = Pair genParams1 genParams2
        m = opModel (*) diff3DModel diff3DModel
        (packing, p0) = sources
        points = V.fromList [ let x = 2**i in V2 x (model m genParams x)
                            | i <- [1, 1.1..10]
                            ]

    let Right fit = leastSquares packing [(points,m)] p0
    print genParams
    print $ unpack packing p0
    print $ unpack packing fit
