module GetHeaders where

import Network.HTTP
import Control.Applicative
import Data.Maybe
import qualified Control.Exception as X
import Network.Stream

download url = liftA rspHeaders <$> runHTTP url

runHTTP url = (simpleHTTP (headRequest url) ) `X.catch` exceptionHandler

exceptionHandler :: X.SomeException -> IO (Result (Response String))
exceptionHandler x = simpleHTTP (headRequest "http://www.awaq.com/")

checkRightOrLeft (Left x) = []
checkRightOrLeft (Right x) = x

-- Header types can be found here:
-- http://hackage.haskell.org/packages/archive/HTTP/latest/doc/html/Network-HTTP-Headers.html#t:HeaderName
contentType' = HdrContentType
lastModified' = HdrLastModified

getHeader x url = fromMaybe "" . lookupHeader x . checkRightOrLeft <$> download url

getLastModified = getHeader lastModified'
