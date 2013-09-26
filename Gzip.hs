module Gzip where

import Types
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)

-- Check if the user asked for Raw=0.0 gzipped data. If so, then return data as gzip. otherwise just return plaintext
checkForGZip :: HeaderNew -> BL.ByteString -> (Bool, BL.ByteString)
checkForGZip input bString
  | gzipFlag input == True = (True, gzip bString)
  | otherwise = (False, bString)
 where
  gzip = compress

