module GetHeaders where

import Network.HTTP
import Control.Applicative
import Data.Maybe
import qualified Control.Exception as X
import Network.Stream

-- Downloads the URL and isolates the response headers
download url = liftA rspHeaders <$> runHTTP url

-- Do a head request, and catch any exceptions.
runHTTP url = (simpleHTTP (headRequest url) ) `X.catch` exceptionHandler

-- IF there is any exception in the HTTP head request, then replace it with a request to awaq.com
exceptionHandler :: X.SomeException -> IO (Result (Response String))
exceptionHandler x = simpleHTTP (headRequest "http://www.awaq.com/")

-- Pattern match on the Either functor
checkRightOrLeft (Left x) = []
checkRightOrLeft (Right x) = x

-- Header types can be found here:
-- http://hackage.haskell.org/packages/archive/HTTP/latest/doc/html/Network-HTTP-Headers.html#t:HeaderName
contentType' = HdrContentType
lastModified' = HdrLastModified

-- Gets headers and isolates a single header type. See contentType' and lasModified' for an example of header type
getHeader x url = fromMaybe "" . lookupHeader x . checkRightOrLeft <$> download url

-- Sugar to get the "lastmodified" header and isolate it out into a string
getLastModified = getHeader lastModified'
