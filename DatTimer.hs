module DatTimer where
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Map.Lazy as M
import Control.Applicative
import System.Directory
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Lazy as BL hiding (readFile, writeFile, pack)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Concurrent
import System.Posix.Files

maxPerSecond :: Int
maxPerSecond = 10

maxPerMinute :: Int
maxPerMinute = 60 

maxPerHour :: Int
maxPerHour = 600 


serverDirectory = "/home/serve/script/"
datsDirectory = serverDirectory ++ "dats/"
minutesDirectory = "/home/serve/script/dats/hours/"
hoursDirectory = "/home/serve/script/dats/minutes/"
secondsDirectory = "/home/serve/script/dats/seconds/"
directoriesToCheck = [serverDirectory, datsDirectory, minutesDirectory, hoursDirectory, secondsDirectory]

secondsFile   :: String -> String
secondsFile x = mconcat [datsDirectory, "seconds/", x, ".dat"]      -- Delete this dat file once every second

minuteFile    :: String -> String
minuteFile  x = mconcat [datsDirectory, "minutes/", x, ".dat"]      -- Delete this dat file once every minute

hourFile      :: String -> String
hourFile    x = mconcat [datsDirectory, "hours/", x, ".dat"]        -- Delete this dat file once every hour


isUserOverDatLimit :: String -> IO Bool
isUserOverDatLimit userHash = checkFilesForLimit fileList
  where
    primeHash = removePunctuation userHash
    fileList = [(secondsFile primeHash, maxPerSecond), (minuteFile primeHash, maxPerMinute), (hourFile primeHash, maxPerHour)]

-- Returns True if the user is over their speedlimit, false if they are not
checkFilesForLimit :: [(FilePath, Int)] -> IO Bool
checkFilesForLimit [] = return False
checkFilesForLimit (x:xs) = do
  overLimit <- checkFileForLimit x
  if overLimit 
    then return True
    else checkFilesForLimit xs

-- Returns True if the user is over their speedlimit, false if they are not
checkFileForLimit :: (FilePath, Int) -> IO Bool
checkFileForLimit (fileName, speedLimit) = do
  existence <- doesFileExist fileName
  if existence
    then do
      fileIn <- BL.readFile fileName
      return . checkSpeed . modify $ fileIn
    else return False
  where
    modify = fst . fromJust . BL.readInt
    checkSpeed = (<) speedLimit

removePunctuation :: String -> String
removePunctuation x = doWork x ""
  where
    doWork [] y = y
    doWork (x:xs) y
      | x == '.' = doWork xs y
      | x == '/' = doWork xs y
      | otherwise = doWork xs (x:y)


addUserToTimer :: String -> IO ()
addUserToTimer userHash = do
  mapM_ (forkIO . addToTimer) fileList
  where
    output = T.pack $ userHash ++ "\n"
    primeHash = removePunctuation userHash
    fileList = [secondsFile primeHash, minuteFile primeHash, hourFile primeHash]

addToTimer :: FilePath -> IO ()
addToTimer fileName = do
  existence <- doesFileExist fileName
  if existence 
    then do
      fileContents <- BL.readFile fileName
      let a = BL.length fileContents
      seq a $ BL.writeFile fileName $ modified fileContents
    else do
      BL.writeFile fileName . BL.pack $ "0"
 where
  modified = BL.pack . show . (+ 1) . fst . fromJust . BL.readInt 
  


