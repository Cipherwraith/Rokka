module Header where
import Data.Monoid
import qualified Data.ByteString.Lazy as BL hiding (pack)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Int
import Types
import Network.URI
import Data.Char

-- Outgoing headers:
--
messageLength = 10
isItZipped = True

buildHeader :: Int -> Bool -> Int64 -> String -> BL.ByteString
buildHeader code isItZipped messageLength lastModified = header code isItZipped messageLength lastModified

-- Build a header with correct content length
header :: Int -> Bool -> Int64 -> String -> BL.ByteString
header code isItZipped messageLength lastModified
  | isItZipped = mconcat [ status, plainText, contentLength, mLength, modified, gzipped, endHeader ]
  | otherwise = mconcat [ status, plainText, contentLength, mLength, modified, endHeader ]
  where
    status = statusCode code
    contentLength = BL.pack "\r\nContent-Length: "
    mLength = BL.pack . show $ messageLength
    gzipped = BL.pack "\r\nContent-Encoding: gzip"
    plainText = BL.pack "\r\nContent-Type: text/plain; charset=\"Shift_JIS\""
    modified = if lastModified == "" 
                  then BL.pack ""
                  else BL.pack ("\r\nLast-Modified: " ++ lastModified)
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


-- Incoming headers:
--
-- Parses the header sent by the user, looks for the GET data
parseHeader :: String -> Header
parseHeader  = (parseHead 1) . lines
  where
  parseHead :: Int -> [String] -> Header
  parseHead i [] = Header "meow"
  parseHead i (h:hs)
    | i > 100 = parseHead (i + 1) []
    | take 3 h == "GET" =  checkForEnd 1 h hs
    | otherwise =  parseHead (i + 1) hs

checkForEnd i headerGet [] = Header "asdf"
checkForEnd i headerGet (h:hs)
  | i > 100 =  Header "lulz"
  | h == ("\r\n\r\n") =  Header headerGet
  | h == ("\r\n") =  Header headerGet
  | h == ("\r") =  Header headerGet
  | h == ("\n") =  Header headerGet
  | otherwise = checkForEnd (i + 1) headerGet hs

parseHeaderStream headerIn = HeaderNew getRequestQuery getGzipFlag getUserAgent getDoesItEnd
  where
    stream :: [String]
    stream = headerStream headerIn

    -- Escape the Url Encoding also!
    getRequestQuery :: (String, String)
    getRequestQuery = parseRequestQuery stream 

    getGzipFlag  :: Bool
    getGzipFlag = parseGzipFlag stream

    getUserAgent :: Maybe String
    getUserAgent = parseUserAgent stream
    
    getDoesItEnd :: Bool
    getDoesItEnd = parseDoesItEnd stream


parseRequestQuery :: [String] -> (String, String)
parseRequestQuery [] = ("asdf", "asdf")
parseRequestQuery (x:xs) 
  | length (words x) < 1 = parseRequestQuery xs
  | "GET" == s = (s, unEscapeString x)
  | "HEAD" == s = (s, unEscapeString x)
  | "POST" == s = (s, unEscapeString x)
  | "PUT" == s = (s, unEscapeString x)
  | "DELETE" == s = (s, unEscapeString x)
  | "TRACE" == s = (s, unEscapeString x)
  | "CONNECT" == s = (s, unEscapeString x)
  | otherwise = parseRequestQuery xs
 where
  s = head $ words x

methods :: [String]
methods = ["GET", "HEAD", "POST", "PUT", "DELETE", "TRACE", "CONNECT"]


parseGzipFlag :: [String] -> Bool
parseGzipFlag [] = False
parseGzipFlag (x:xs) 
  | ("accept-encoding" `elem` s) && ("gzip" `elem` s) = True
  | otherwise = parseGzipFlag xs
 where
    -- remove commas, then make everything lowercase, and finally split by word
    s = words $ removePunctuation trimmed ""

    -- take only 100 chars to be parsed
    trimmed = take 100 x

    -- recursive function to remove punctuation and replace them with a space
    removePunctuation [] s = reverse s
    removePunctuation (z:zs) s
      | z == ',' = removePunctuation zs (' ' : s)
      | z == ';' = removePunctuation zs (' ' : s)
      | z == ':' = removePunctuation zs (' ' : s)
      | otherwise = removePunctuation zs ((toLower z):s) 
    


parseUserAgent :: [String] -> Maybe String
parseUserAgent [] = Nothing
parseUserAgent (x:xs) 
  | "User-Agent:" `elem` s = Just x
  | otherwise = parseUserAgent xs
 where
  s = words x

parseDoesItEnd :: [String] -> Bool
parseDoesItEnd [] = False
parseDoesItEnd (x:xs)
  | x == "\r" = True
  | otherwise = parseDoesItEnd xs


-- Gets a header without waiting to be blocked. It should work with a stream of header info
headerStream :: String -> [String]
headerStream headerIn = getHeaderStream (lines $ headerIn) [] 0
  where
    getHeaderStream [] m _ = m
    getHeaderStream (h:hs) m i 
      | i > 50 = m
      | h == "\r" = (h:m)
      | otherwise = getHeaderStream hs (h:m) i


testHeader1 = "GET /path/to/file/index.html HTTP/1.0\r\nUser-agent: Mozilla/3.0Gold\r\nAccept-Encoding: gzip\r\n\r\n"

testHeader2 = "GET http://www.tiggerwigger.com/ HTTP/1.0\r\nProxy-Connection: Keep-Alive\r\nUser-Agent: Mozilla/5.0 [en] (X11; I; Linux 2.2.3 i686)\r\nHost: www.tiggerwigger.com\r\nAccept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, \r\nAccept-Encoding: gzip\r\nAccept-Language: en\r\nAccept-Charset: iso-8859-1, *, utf-8\r\n\r\n"

testHeader3 = "GET http://www.tiggerwigger.com/ HTTP/1.0\r\nProxy-Connection: Keep-Alive\r\nHost: www.tiggerwigger.com\r\nAccept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, \r\nAccept-Encoding: zip\r\nAccept-Language: en\r\nAccept-Charset: iso-8859-1, *, utf-8\r\n\r\n"

testHeader4 = "GET http://www.tiggerwigger.com/ HTTP/1.0\r\nProxy-Connection: Keep-Alive\r\nHost: www.tiggerwigger.com\r\nAccept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, \r\nAccept-Encoding: zip\r\nAccept-Language: en\r\nAccept-Charset: iso-8859-1, *, utf-8\r\n"

testHeader5 = ""

testHeader6 = "HEAD http://www.tiggerwigger.com/ HTTP/1.0\r\nProxy-Connection: Keep-Alive\r\nHost: www.tiggerwigger.com\r\nAccept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, \r\nAccept-Encoding: zip\r\nAccept-Language: en\r\nAccept-Charset: iso-8859-1, *, utf-8\r\n"

