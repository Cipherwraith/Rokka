module Utility where
import Network.URI
import System.Directory
import Data.Char
import Types

-- Take an encoded url and decode it
urlDecode :: Header -> Header
urlDecode = Header . unEscapeString . getData

-- Converts a string to Int. Make sure the string only contains numbers first!
convertToInt x = read filtered :: Int
  where
    filtered = if (length $ filter isNumber x) > 0
                  then filter isNumber x -- is number
                  else "0" -- is not number

fst' (x,_,_) = x
snd' (_,y,_) = y
thd' (_,_,z) = z

checkPaths x = do
  existence <- doesDirectoryExist x
  if existence
    then return ()
    else do
      createDirectoryIfMissing True x
      return ()

