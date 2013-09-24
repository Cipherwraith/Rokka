module Main where
import System.Posix.Time
import System.Directory
import Control.Concurrent
import Data.List

secondsDirectory :: String
secondsDirectory = "/home/serve/script/dats/seconds/"

main :: IO ()
main = do
  removeSecondsDats

removeSecondsDats = do
  threadDelay 1000000 
  contents <- getDirContents secondsDirectory
  mapM_ removeFile contents
  removeSecondsDats

getDirContents x = do
  c <- getDirectoryContents x
  return $ modified c
  where
    modified = map (x ++) . delete "." . delete ".."
