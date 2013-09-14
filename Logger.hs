module Logger where

import System.Directory
import Data.Time.Clock
import Data.Time.Calendar
import Control.Applicative
import Data.Monoid

getDate :: IO String
getDate = dateToString . toGregorian . utctDay <$> getCurrentTime
  where
    dateToString (year, month, day) = mconcat [formattedYear, "-", formattedMonth, "-", formattedDay, ".txt"]
      where
        formattedDay = if day < 10 
                          then "0" ++ show day
                          else show day
        formattedMonth = if month < 10 
                          then "0" ++ show month
                          else show month
        formattedYear = show year

directory :: String
directory = "/home/serve/script/logs/"

logFile :: String -> String -> String 
logFile prefix date = mconcat [directory, prefix, "-", date]


toLog :: String -> String -> IO ()
toLog prefix content = do
  existence <- doesDirectoryExist directory
  if not existence
    then do
          createDirectoryIfMissing False directory
          date <- getDate
          appendFile (logFile prefix date) (content ++ "\n")
    else do
          date <- getDate
          appendFile (logFile prefix date) (content ++ "\n")
  

