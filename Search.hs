module Search where
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Search as BL
import qualified Data.ByteString.Lazy.Char8 as BL  hiding (split)
import qualified Data.ByteString.Lazy as BL hiding (readFile, pack, split)
import Debug.Trace

meow = BL.pack "Hello" -- Dat file exists
meow2 = BL.pack "haha <html>" -- Dat file doesnt exist

datFileCheck1 :: BL.ByteString -> Bool
-- if <html> is found within the first 150 chars, then its most likely a 404 not found
datFileCheck1 x = not safeInx
  where
    html = B.pack "<html>"
    -- checks if <HTML> exists in x, if it does it returns a list of the locations
    inx = BL.indices html x
    -- makes sure inx has elements. if it does then checks the head and sees if it is less than 150. if so then return false
    safeInx
      | length inx <= 0 = True -- if there are no elements in inx, then return True (it didnt find <html>)
      | otherwise = head inx > 150 -- otherwise return whether or not it finds html

datFileCheck2 :: BL.ByteString -> Bool
-- if <html> is found within the first 150 chars, then its most likely a 404 not found
datFileCheck2 x = not safeInx
  where
    html = B.pack "<HTML>"
    -- checks if <HTML> exists in x, if it does it returns a list of the locations
    inx = BL.indices html x
    -- makes sure inx has elements. if it does then checks the head and sees if it is less than 150. if so then return false
    safeInx
      | length inx <= 0 = True -- if there are no elements in inx, then return True (it didnt find <html>)
      | otherwise = head inx > 150 -- otherwise return whether or not it finds html



datFileExistence :: BL.ByteString -> Bool
datFileExistence datFile = (datFileCheck1 datFile) || (datFileCheck2 datFile)
