module Mosaic where
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Search as BL 
import qualified Data.ByteString.Lazy.Char8 as BL  hiding (split)
import qualified Data.ByteString.Lazy as BL hiding (readFile, pack, split)
import Control.Applicative
import Data.Monoid
import Control.Monad



testFile = BL.readFile "1329750184.dat"


searchB = BL.indices (B.pack "<>2013/08")
searchA = BL.indices (B.pack "<>2013/07")


testByteString = head. BL.lines <$> testFile

replaceDates :: BL.ByteString -> BL.ByteString
replaceDates bString = stepThrough splitString (BL.pack "")
  where
    splitString = BL.lines bString

    stepThrough :: [BL.ByteString] -> BL.ByteString -> BL.ByteString
    stepThrough [] o = o
    stepThrough (b:bs) o
      | null (searchA b) && null (searchB b) = stepThrough bs (mconcat [o, b, n])
      | otherwise = stepThrough bs (mconcat [o,removeTimestamp b,n])

    n :: BL.ByteString
    n = BL.pack "\n"

    -- remove the timestamp from a bytestring
    removeTimestamp :: BL.ByteString -> BL.ByteString
    removeTimestamp s 
      | not (length splitByDelimiter > 4) = s -- make sure there are 5 or more delimited by "<>"
      | not (length splitBySpace == 3) = s -- make sure there are exactly 3 delimited by " " in the timestamp and date field
      | otherwise = rebuildByteString -- rebuild the bytestring with the new "NY:AN:NY.AN" instead of the bytestring
     where
      splitByDelimiter = BL.split (B.pack "<>") s -- Split string on "<>"
      stringWithTimeStamp = splitByDelimiter !! 2 -- Take the time and date section 
      splitBySpace = BL.split (B.pack " ") stringWithTimeStamp -- Split time and date by space
      mosaicedTimestamp = BL.pack "NY:AN:NY.AN" -- Replace the timestamp with nyannyan
      rebuildTimeAndDate = mconcat [head splitBySpace, sp, mosaicedTimestamp, sp, head . reverse $ splitBySpace] -- Build it all back up
      rebuildByteString = mconcat [splitByDelimiter !! 0, dl, splitByDelimiter !! 1, dl, rebuildTimeAndDate, dl, BL.intercalate dl $ drop 3 splitByDelimiter]
      sp = BL.pack " "
      dl = BL.pack "<>"
