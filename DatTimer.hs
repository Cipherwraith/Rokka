module DatTimer where
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Map.Lazy as M
import Control.Applicative
import System.Directory
import Data.Maybe

serverDirectory = "/home/serve/script/"

secondsFile   :: String
secondsFile   = serverDirectory ++ "seconds.dat"      -- Delete this dat file once every second

minuteFile    :: String
minuteFile    = serverDirectory ++ "minutes.dat"      -- Delete this dat file once every minute

hourFile      :: String
hourFile      = serverDirectory ++ "hours.dat"        -- Delete this dat file once every hour

maxPerSecond :: Int
maxPerSecond = 10

maxPerMinute :: Int
maxPerMinute = 60 

maxPerHour :: Int
maxPerHour = 600 

-- if user is over their dat limit, then this will return True
--isUserOverDatLimit :: String -> IO Bool
isUserOverDatLimit :: String -> IO Bool
isUserOverDatLimit userHash = do
  sList <- secondsListCount -- Amount of times user accessed in last second
  if fromMaybe 1 sList > maxPerSecond -- check if user is over max second limit
    then return True -- if over then return true
    else do
      mList <- minutesListCount
      if fromMaybe 1 mList > maxPerMinute  -- if not over, then check the minute limit
        then return True
        else do
          hList <- hourListCount 
          return $ fromMaybe 1 hList > maxPerHour -- if not over, then check the hour limit
  where
    secondsListCount :: IO (Maybe Int)
    secondsListCount = M.lookup userName <$> secondsList

    minutesListCount :: IO (Maybe Int)
    minutesListCount = M.lookup userName <$> minutesList

    hourListCount :: IO (Maybe Int)
    hourListCount = M.lookup userName <$> hourList

    userName :: T.Text
    userName = T.pack userHash

addUserToTimer :: String -> IO ()
addUserToTimer userHash = do
  T.appendFile secondsFile output
  T.appendFile minuteFile output
  T.appendFile hourFile output
  where
    output = T.pack $ userHash ++ "\n"

secondsList :: IO (M.Map T.Text Int)
secondsList = do
  existence <- doesFileExist secondsFile
  if existence
    then do
      fileIn <- T.readFile secondsFile
      let modFile = M.fromListWith (+) $ zip (T.lines fileIn) [1,1..]
      return modFile
    else return basicMap

minutesList :: IO (M.Map T.Text Int)
minutesList = do
  existence <- doesFileExist minuteFile
  if existence
    then do
      fileIn <- T.readFile minuteFile
      let modFile = M.fromListWith (+) $ zip (T.lines fileIn) [1,1..]
      return modFile
    else return basicMap

hourList :: IO (M.Map T.Text Int)
hourList = do
  existence <- doesFileExist hourFile
  if existence
    then do
      fileIn <- T.readFile hourFile
      let modFile = M.fromListWith (+) $ zip (T.lines fileIn) [1,1..]
      return modFile
    else return basicMap

basicMap :: M.Map T.Text Int
basicMap = M.fromList [(T.pack "", 0)]


