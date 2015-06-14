module Utils ( intToByteString
             ) where

import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (intDec, toLazyByteString)

intToByteString :: Int -> B.ByteString
intToByteString = toStrict . toLazyByteString . intDec
