module Header where
import Data.Monoid
import qualified Data.ByteString.Lazy as BL hiding (pack)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Int

messageLength = 10
isItZipped = True

buildHeader :: Int -> Bool -> Int64 -> BL.ByteString
buildHeader code isItZipped messageLength = header code isItZipped messageLength  

-- Build a header with correct content length
header :: Int -> Bool -> Int64 -> BL.ByteString
header code isItZipped messageLength
  | isItZipped = mconcat [ status, contentLength, mLength, gzipped, endHeader ]
  | otherwise = mconcat [ status, contentLength, mLength, endHeader ]
  where
    status = statusCode code
    contentLength = BL.pack "\r\nContent-Length: "
    mLength = BL.pack . show $ messageLength
    gzipped = BL.pack "\r\nContent-Encoding: gzip"
    plainText = BL.pack "\r\nContent-Type: plain/text"
    endHeader = BL.pack "\r\n\r\n"

statusCode :: Int -> BL.ByteString
statusCode code = BL.pack $ check code
 where
  check code
    | code == 200 = "HTTP/1.0 200 OK"
    | code == 401 = "HTTP/1.0 401 Unathorized"
    | code == 400 = "HTTP/1.0 400 Bad Request"
    | code == 404 = "HTTP/1.0 404 Not Found"
    | otherwise = "HTTP/1.0 403 Forbidden"

