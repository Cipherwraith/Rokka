module Errors where
import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL

{-- -- Alternative error messages
inputError = BL.pack "404 Not Found - Please check input and try again"
authenticationError = BL.pack "401 Unauthorized - Authentication Error"
urlError = BL.pack "400 Bad Request - Check URL and try again"
timeLimitError = BL.pack "401 Unauthorized - Request Limit Reached, please wait and try again"
--}


-- Errors.
inputError :: (BL.ByteString, Int, String)
inputError = (BL.pack "Error 8008135", 404, "")

authenticationError :: (BL.ByteString, Int, String)
authenticationError = (BL.pack "Error 69", 401, "")

urlError :: (BL.ByteString, Int, String)
urlError = (BL.pack "Error 666", 400, "")

timeLimitError :: (BL.ByteString, Int, String)
timeLimitError = (BL.pack "Error 420", 401, "")

pageDoesNotExistError :: (BL.ByteString, Int, String)
pageDoesNotExistError = (BL.pack "Error 13", 404, "")


-- This is a full output of an error code, header, and message 
error405NotAllowed :: BL.ByteString
error405NotAllowed = BL.pack $ "HTTP/1.0 405 Method Not Allowed\r\nContent-Type: text/plain; charset=\"Shift_JIS\"\r\nContent-Length: 8\r\n\r\nError 42"

error404NotFound :: BL.ByteString
error404NotFound = BL.pack $ "HTTP/1.0 404 Not Found\r\nContent-Type: text/plain; charset=\"Shift_JIS\"\r\nContent-Length: 8\r\n\r\nError 13"
