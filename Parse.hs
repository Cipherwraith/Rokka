module Parse where 

import Prelude hiding (error)

import Control.Arrow hiding (loop)
import qualified Data.Set as S
import Data.List.Split
import Data.Char
import Data.Maybe

import Types
import ChanBoards
import Utility

{--
    End options
    /4   (st=4&to=4)      Only one post
    /4-  (st=4)           Starting at a certain post number
    /-6  (to=6)           Stopping at a certain post number
    /4-6 (st=4to=4)       Between two posts
    /l10 (ls=10)          Last n posts
    /i   (imode=true)     mobile mode
    /.   (nofirst=false)  keep the first post
    /n   (nofirst=true)   remove the first post
 --}

-- Going to split the string and get the server,board,and post number. 
-- If these all exist and are correct, then we will check for the "end options"
-- If there are end options, we will parse them and return them in the data type.
-- If a board or server dont match the premade list, then everything is set to Nothing, and an error is output
-- 
--    Sample Header data:
--      /pele/hnews/123123123/l50&raw=0.0&sid=Monazilla/2.00:437576303V875807Q65482S5373415V0353657X819589B683935C83892l0684065u718984C13042Y073615439W33071V8555402N76303M0122748U5567915F128809I381065V6928103Q47334M0251341Y65808j5567915e7"
      
parseInput :: HeaderNew -> Input
parseInput input = if fst getServer && fst getBoard && fst getPost 
                      then Input 
                              (snd getServer) 
                              (snd getBoard) 
                              (snd getPost) 
                              getRaw 
                              getSID 
                              (fst parseStartStop) 
                              (snd parseStartStop) 
                              getL 
                              getKeepFirst 
                              getRemoveFirst 
                              Nothing 
                              Nothing 
                      else Input 
                              Nothing 
                              Nothing 
                              Nothing 
                              Nothing 
                              Nothing 
                              Nothing 
                              Nothing 
                              Nothing 
                              Nothing 
                              Nothing 
                              (Just True) 
                              (Just "Server, board, or post were wrong") 
  where

    -- Checks for the "HEAD" http flag.
    checkIfHeadOnly
      | length (snd $ requestQuery input) < 4 = Nothing
      | take 4 (snd $ requestQuery input) == "HEAD" = Just True
      | otherwise = Just False

    checkIfItsGet
      | length (snd $ requestQuery input) < 3 = Nothing
      | take 3 (snd $ requestQuery input) == "GET" = Just True
      | otherwise = Just False

    -- parse the data for the "l50" at the end of the post
    getL :: Maybe Int
    getL = if 'l' `elem` separateOptions -- if 'l' exists in the opts
                  then parseForL separateOptions -- parseForL 
                  else Nothing
      where
        parseForL :: String -> Maybe Int
        parseForL opts = if null $ filterL opts
                            then Nothing
                            else Just . convertToInt . filterL $ opts
        filterL :: String -> String
        filterL = filter isNumber . concat . drop 1 . splitOn "l"

    getSID :: Maybe String
    getSID
      | length splitByQuestionMark < 2 = Nothing -- Make sure there is a question mark first
      | length sid /= 196 = Nothing -- Make sure the SID is 196 chars long including the "sid="
      | take (length headerSID) sid /= headerSID = Nothing -- Make sure the first 19 chars are "sid=Monazilla/2.00:"
      | otherwise = Just sid 
     where
      headerSID :: String
      headerSID = "sid=Monazilla/2.00:"

      questioned :: String
      questioned = if length splitByQuestionMark > 1
                     then concat $ drop 1 splitByQuestionMark
                     else "" 

      -- get the sid
      sid :: String
      sid = if getRaw == Nothing -- check if getRaw is empty
              then questioned -- if its empty, just return the string split by question mark
              else if length (splitOn "&" questioned) > 1 --otherwise check if a "&" exists
                then concat . drop 1 $ splitOn "&" questioned -- then split by "&" and return the second unit
                else "" -- else just return nothing
      
    -- parse the data for the "raw=0.0" stuff (its useless now, but will be changed later)
    getRaw :: Maybe String
    getRaw 
      | not (elem '?' inputData) = Nothing -- make sure question mark exists before splitting
      | length splitByQuestionMark < 2  = Nothing -- Make sure the list has at least 2 entries
      | not (elem '&' (splitByQuestionMark !! 1)) = Nothing -- Makes sure an ampersand exists
      | take 3 (splitByQuestionMark !! 1) == "raw" = Just (head . splitOn "&" $ splitByQuestionMark !! 1) -- check if the first three characters are 'raw', if so, then split it at the ampersand and return the head
      | otherwise = Nothing
    
    splitByAmp = splitOn "&" inputData
    splitByQuestionMark = splitOn "?" inputData

    -- Define whether or not we keep the first post based on getRemoveFirst's decision
    getKeepFirst 
      | isNothing getRemoveFirst = Just True -- If getRemoveFirst is Nothing, then keep the first post
      | fromJust getRemoveFirst = Just False -- if getRemoveFirst is True, we dont keep the first post 
      | otherwise = Just True -- otherwise, we keep the first post
    
    -- Check whether or not an 'n' is in the options
    getRemoveFirst = Just $ 'n' `elem` separateOptions

    -- Parse for the "start" and "stop" options, return in a tuple
    parseStartStop :: (Maybe Int, Maybe Int)
    parseStartStop 
      | isJust getL = (Nothing, Nothing) -- If getL is a Just, then we return nothing. If getL is nothing, we continue
      | '-' `elem` separateOptions = checkForStartStop -- if options has a '-' then check for start and stop
      | null (map isNumber (filter isNumber separateOptions)) = (Nothing, Nothing) -- If no numbers in options, then return nothing
      | otherwise = (Just . convertToInt &&& Just . convertToInt) (filter isNumber separateOptions) -- return a single number
     where
      checkForStartStop = if length splitForDash == 2
                              then numberLogic splitForDash -- filter non numbers from the two elements
                                                              -- if the first number is "", then set it to Nothing
                                                              -- if the second number is "", then set it to Nothing
                              else (Nothing,Nothing)

    -- Split on dash to help separate options
    splitForDash :: [String]
    splitForDash = map (filter isNumber) . take 2 $ splitOn "-" separateOptions -- take2 is okay, because we already confirmed that a '-' exists

    -- numberlogic takes a list with two strings in it, checks if they are empty, converts them to integers, then outputs as a tuple
    numberLogic :: [String] -> (Maybe Int, Maybe Int)
    numberLogic n = (f &&& b) n
     where
      f n = if head n == "" -- Check the first number to see if it exists. return Nothing if it doesnt
              then Nothing
              else Just . convertToInt . head $ n
      b n = if (head . drop 1) n == "" -- Check the second number to see if it exists. return Nothing if it doesnt
              then Nothing
              else Just . convertToInt . head . drop 1 $ n


    separateOptions :: String
    separateOptions  = if length splitBySlashes < 4  -- Make sure there are enough things before doing the (!!)
                        then ""
                        else if '?' `elem` splitPiece -- Make sure a question mark exists
                          then head . splitOn "?" $ splitPiece
                          else ""
      where
        splitPiece = splitBySlashes !! 4

    getServer :: (Bool, Maybe String)
    getServer 
      | length splitBySlashes < 4  = (False, Nothing) -- Check if there are more than 4 Strings in the list
      | (splitBySlashes !! 1) `S.member` listOfServers = (True, Just $ splitBySlashes !! 1)-- Check if the first in the list (ie pele) is in the server list
      | otherwise = (False, Nothing) -- If not, then return with nothing

    getBoard :: (Bool, Maybe String)
    getBoard 
      | length splitBySlashes < 4 = (False, Nothing) -- Check if there are more than 4 Strings in the list
      | (splitBySlashes !! 2) `S.member` listOfBoards = (True, Just $ splitBySlashes !! 2) -- check if second word is in the board list
      | otherwise = (False, Nothing) -- If not, then return with nothing

    getPost :: (Bool, Maybe String)
    getPost 
      | length splitBySlashes < 4 = (False, Nothing)-- Check if there are more than 4 Strings in the list
      | False `elem` map isNumber (splitBySlashes !! 3) = (False, Nothing) -- Check if d!!3 is a number
      | otherwise = (True, Just (splitBySlashes !! 3))

    splitBySlashes = splitOn "/" inputData
    inputData = concat . take 1 . drop 1 . splitOn " " . snd . requestQuery $ input :: String

blankInput :: Input
blankInput = Input Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 


