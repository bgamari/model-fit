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

sources :: Num a => Product Diff3DParams Diff3DParams (Param a)
sources = runParamsM $ do
    diff1 <- param
    diff2 <- param
    aspect <- param
    a <- Diff3DP <$> pure diff1 <*> fixed 1 <*> pure aspect <*> param
    b <- Diff3DP <$> pure diff2 <*> fixed 1 <*> pure aspect <*> param
    return $ Pair a b

main = do
    let genParams1 = defaultParams & (diffTime      .~ 10)
                                   . (concentration .~ 2)
        genParams2 = defaultParams & (diffTime      .~ 30)
                                   . (concentration .~ 2)
        genParams = Pair genParams1 genParams2
        m = opModel (*) diff3DModel diff3DModel
        points = V.fromList [ let x = 2**i in V2 x (model m genParams x)
                            | i <- [1, 1.1..10]
                            ]
        --ls = embedParams sources
    --let p0 = (Packed $ V.replicate 3 0) & ls (_1 . diffTime) .~ 10
    let p0 = Packed $ V.fromList [1,3,5,6,4]
    let Right fit = leastSquares points sources m p0
    print genParams
    print $ unpack sources fit
