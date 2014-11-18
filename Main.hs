import Data.Foldable as F
import Data.Functor.Identity
import qualified Data.Vector as V
import qualified Data.IntMap as IM

import Linear
import Control.Lens

import Model       
import FcsModels
import FcsFit

main = do
    let genParams = defaultParams & (diffTime      .~ 10)
                                  . (concentration .~ 2)
        params = genParams
        m = diff3DModel
        points = V.fromList [ let x = 2**i in Point (V1 x) (model m genParams $ V1 x) (V1 1)
                            | i <- [1, 1.1..5] ]
        sources = Diff3DP (FromVector $ PIdx 0)
                          (Fixed 1)
                          (FromVector $ PIdx 1)
                          (FromVector $ PIdx 2)
        packedParams = PP $ IM.fromList $ zip [0..] [5,3,1]

    let p = Identity genParams in print $ (chiSquared m p (Identity points), runIdentity p)
    let fits = takeEvery 200 $ fit m (Identity points) (Identity sources) packedParams
    F.forM_ fits $ \p->do
        print (chiSquared m p (Identity points), runIdentity p)

takeEvery :: Int -> [a] -> [a]
takeEvery n (x:xs) = x : takeEvery n (drop n xs)
