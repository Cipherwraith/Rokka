module BuildUrl where

import Prelude hiding (error)
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import ChanBoards
import Debug.Trace

-- Build the url to download the post dat. Need the serverName, boardName, and postNumber to start.
buildUrl :: String -> Maybe String -> Maybe String -> Maybe String -> Maybe String
buildUrl poolOrOyster serverN boardN postN 
    = if isNothing serverN || isNothing boardN || isNothing postN -- if neither board, server, nor posts are set, then nothing
        then Nothing
        else Just checkUrlStyle -- $ "http://" ++ fromJust serverN ++ ".bbspink.com/vault/" ++ fromJust boardN ++ "/oyster/" ++ postFirstFour ++ "/" ++ fromJust postN ++ ".dat"
  where
    postFirstThree :: String
    postFirstThree = if length (fromJust postN) > 3
                      then take 3 $ fromJust postN
                      else ""
    postFirstFour :: String
    postFirstFour = if length (fromJust postN) > 4 
                      then take 4 $ fromJust postN
                      else ""
    postFirstFive :: String
    postFirstFive = if length (fromJust postN) > 5
                      then take 5 $ fromJust postN
                      else ""

    checkUrlStyle :: String
    checkUrlStyle
      | fromJust serverN `elem` serverStyle1 = trace (urlStyle1) urlStyle1
      | fromJust serverN `elem` serverStyle2 = trace urlStyle2 urlStyle2
      | fromJust serverN `elem` serverStyle3 = trace urlStyle3 urlStyle3
      | fromJust serverN `elem` serverStyle4 = trace urlStyle4 urlStyle4
      | fromJust serverN `elem` serverStyle5 = trace urlStyle5 urlStyle5
      | otherwise = trace urlStyle6 urlStyle6

    -- pele.bbspink.com/vault/_datArea/erobbs/oyster/1285/1285357421.dat
    -- BBSPink Pool
    urlStyle1 :: String
    urlStyle1 = mconcat ["http://", fromJust serverN, ".bbspink.com/vault/_datArea/", fromJust boardN, "/", checkForPool, "/",  fromJust postN, ".dat"]
    
    -- BBSPink Archives
    urlStyle2 :: String
    urlStyle2 = mconcat ["http://", fromJust serverN, ".bbspink.com/vault/", fromJust boardN, "/", postFirstFour, "/", postFirstFive, "/", fromJust postN, ".dat.gz"]

    -- BBSPink Archives
    urlStyle3 :: String
    urlStyle3 = mconcat ["http://", fromJust serverN, ".bbspink.com/vault/", fromJust boardN, "/kako/", postFirstFour, "/", postFirstFive, "/", fromJust postN, ".dat"]

    -- BBSPink Archives
    urlStyle4 :: String
    urlStyle4 = mconcat ["http://", fromJust serverN, ".bbspink.com/vault/", fromJust boardN, "/kako/", postFirstThree, "/", fromJust postN, ".dat"]

    -- 2ch Pool
    urlStyle5 :: String
    urlStyle5 = mconcat ["http://", fromJust serverN, ".2ch.net/vault/_datArea/", fromJust boardN, "/", checkForPool, "/", fromJust postN, ".dat"]

    -- 2ch Archives
    urlStyle6 :: String
    urlStyle6 = mconcat ["http://", fromJust serverN, ".hanako.2ch.net/vault/", fromJust boardN, "/oyster/", postFirstFour, "/", fromJust postN, ".dat"]

    -- Check if pool or oyster. If it is oyster, then append the post's first four dat numbers.
    checkForPool 
      | poolOrOyster == "oyster" = mconcat [poolOrOyster, "/", postFirstFour]
      | otherwise = poolOrOyster 

    {--
    serverName2ch :: Maybe String
    serverName2ch 
      | isNothing lookedUp = serverN -- Check if the servername is in the archived server list
      | otherwise = lookedUp -- If it is, then return the proper name for banana3000
     where
      name' = fromJust serverN
      lookedUp = M.lookup name' serverMap

    serverStyle1 :: [String]
    serverStyle1 = ["babiru","qiufen","idol","set","venus","yomi","sakura01","peach","sakura02","pie","kilauea","pele"]

    serverStyle2 :: [String]
    serverStyle2 = ["vip"]

    serverStyle3 :: [String]
    serverStyle3 = ["okazu","wow","pink"]

    serverStyle4 :: [String]
    serverStyle4 = ["www2"]

    serverStyle5 :: [String]
    serverStyle5 = ["hayabusa","hayabusa2","yuzuru","hibari","awabi","kamome","ikura","toro","hato","anago","engawa","qb5","qb7","kohada","uni"]
--}


