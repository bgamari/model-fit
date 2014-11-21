module CsvUtils where

import Data.Char (ord)
import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL
import Data.Csv as Csv
import Linear

decodePoints :: FromRecord a => BSL.ByteString -> Either String (V.Vector a)
decodePoints = Csv.decodeWith decOpts HasHeader
  where decOpts = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ' ' }

readPoints :: FromRecord a => FilePath -> EitherT String IO (V.Vector a)
readPoints path =
    liftIO (BSL.readFile path) >>= EitherT . return . decodePoints

instance FromField a => FromField (V1 a) where
    parseField f = V1 <$> parseField f

instance FromField a => FromField (V2 a) where
    parseField f = V2 <$> parseField f <*> parseField f