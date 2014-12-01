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

sources pts = runFitM $ do
    diff1 <- param 2.3
    diff2 <- param 3.2
    aspect <- param 10
    fit pts (opModel (*) diff3DModel diff3DModel) $
      let a = Diff3DP (pure diff1) (fixed 1) (pure aspect) (param 1)
          b = Diff3DP (pure diff2) (fixed 1) (pure aspect) (param 1)
      in Pair a b

main = do
    let genParams1 = defaultParams & (diffTime      .~ 10)
                                   . (concentration .~ 2)
        genParams2 = defaultParams & (diffTime      .~ 30)
                                   . (concentration .~ 2)
        genParams = Pair genParams1 genParams2
        m = opModel (*) diff3DModel diff3DModel
        (curves, p0, unpack) = sources points
        points = V.fromList [ let x = 2**i in Point x (model m genParams x) 1
                            | i <- [1, 1.1..10]
                            ]

    let Right fit = leastSquares curves p0
    print genParams
    print $ unpack p0
    print $ unpack fit
