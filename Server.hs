-- Name:          Rokka server
-- Author:        CipherWraith
-- Contact:       twitter.com/codemonkeyz
-- Last modified: Sat Sep 21 20:15:45 2013



import Prelude hiding (error)

import Network
import Network.HTTP.Conduit
import Network.URI

import Control.Applicative
import Control.Concurrent
import Control.Exception as X
import Control.Arrow hiding (loop)

import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding

import Data.Monoid
import Data.List.Split
import Data.Char
import Data.Maybe

import System.Random
import System.IO
import System.Posix.Time
import Debug.Trace

import qualified Data.Set as S
import qualified Data.Map as M

import Codec.Compression.GZip

import Header
import Types
import Auth
import Crypt
import DatTimer
import Ops
import Logger
import Mosaic
import ChanBoards
import Search

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 80
  print "Welcome to the inside of the vault"
  loop sock

loop sock = do
  (h,x,z) <- accept sock
  currTime <- epochTime
  rand <- randomRIO (1000000,9999999) :: IO Int
  toLog "ip" $ mconcat [show currTime, " ", encryptT x rand]
  -- process one line at a time
  hSetBuffering h LineBuffering
  print (h,x,z)
  forkIO $ body h
  loop sock

-- Main logic loop
body h = do
  -- Get the incoming header
  headerGot <- hGetContents h :: IO String

  -- get the $_GET data and put it into a data type
  let getInput = parseHeader headerGot :: Header
  currTime <- epochTime
  toLog "input" $ mconcat [show currTime, " ", show getInput] 
  print getInput

  -- unescapes the decoded strings
  let decoded = urlDecode getInput :: Header

  -- Parse the $_GET data, and figure out which board/post/server/sid the user is using
  let input = parseInput decoded :: Input
  print input -- print to terminal
  toLog "parsed" $ mconcat [show currTime, " ", show input]
  
  -- Create the outgoing message with input 
  (msg, code) <- getMsg input :: IO (BL.ByteString, Int)

  -- Check if the input asks for raw=0.0. If it does, then gzip the message
  -- returns True if the message is zipped, and False if not. Message is in the snd
  let possiblyCompressedMessage = checkForGZip input msg :: (Bool, BL.ByteString)

  -- Check if the message is zipped or not
  let isItZipped = fst possiblyCompressedMessage

  -- Get the message into a new variable
  let msg' = snd possiblyCompressedMessage

  -- Get the length of the message:
  let messageLength = BL.length msg' 

  -- Build the outgoing header
  let outgoingHeader = buildHeader code isItZipped messageLength :: BL.ByteString
  toLog "headers" $ mconcat [show currTime, BL.unpack outgoingHeader]

  -- For debug
  toLog "debug" $ mconcat [show currTime, " Gzip: ", show isItZipped, " | Content length: ", show messageLength]

  -- Output the data to the user
  BL.hPut h $ mconcat [outgoingHeader, msg'] -- debug details
  
  -- Clear the handle, and close it
  hFlush h
  hClose h
    where
      -- Gets the header sent by the user
      incomingHeader = BL.pack . getData. parseHeader 

-- Check if the user asked for Raw=0.0 gzipped data. If so, then return data as gzip. otherwise just return plaintext
checkForGZip :: Input -> BL.ByteString -> (Bool, BL.ByteString)
checkForGZip input bString 
  | (raw input) == Nothing = (False, bString) -- If raw input is nothing, then just return the bstring
  | fromJust (raw input) == "raw=0.0" = (True, gzip bString) -- if input is raw=0.0, then gzip bstring
  | otherwise = (False, bString) -- otherwise just return the bstring
 where
  gzip = compress

-- Take an encoded url and decode it
urlDecode :: Header -> Header
urlDecode = Header . unEscapeString . getData


blankInput :: Input
blankInput = Input Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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
      
parseInput :: Header -> Input
parseInput input = if fst getServer && fst getBoard && fst getPost 
                      then Input (snd getServer) (snd getBoard) (snd getPost) getRaw getSID (fst parseStartStop) (snd parseStartStop) getL getKeepFirst getRemoveFirst Nothing Nothing
                      else Input Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just True) (Just "Server, board, or post were wrong")
  where
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
    inputData = concat . take 1 . drop 1 . splitOn " " . getData $ input :: String

-- Converts a string to Int. Make sure the string only contains numbers first!
convertToInt x = read filtered :: Int
  where
    filtered = if (length $ filter isNumber x) > 0
                  then filter isNumber x -- is number
                  else "0" -- is not number
                  
-- Parses the header sent by the user, looks for the GET data
parseHeader :: String -> Header
parseHeader = parseHead . lines 
  where
    parseHead :: [String] -> Header
    parseHead [] = Header "asdf"
    parseHead (h:hs) = if take 3 h == "GET"
                          then checkForEnd h hs
                          else parseHead hs
    checkForEnd headerGet [] = Header "asdf"
    checkForEnd headerGet (h:hs) 
      | h == ("\r\n\r\n") || ("\r\n") = Header h
      | otherwise = checkForEnd headerGet hs

{--
 -- Sample get data
       http://offlaw.bbspink.com/pele/hnews/123123123/l50&raw=0.0&sid=Monazilla/1.00:HCBCGCGCIDHCHEAC@GGDMDOF@CFCCDEGECEGBDDG@DBCGFGFDGFCHEBEHCBCGCGCAEFFLBNCBGAGHFAF@G@G@G@GDCACCCGCICFCGCGCNGBCHDNCFEDFEC@F@C@C@C@CCGGEJFODAGIDIFAG@CECBCHCHEJGDEDBHCACCCGC@CBCHCBCC
   --}

{--
  -- Sample incoming header
  
        GET /asdfasdfasdf HTTP/1.1
        Host: 127.0.0.1
        User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:23.0) Gecko/20100101 Firefox/23.0
        Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
        Accept-Lang
--}


{--
-- Build a header with correct content length
header isItZipped messageLength
  | isItZipped = BL.pack "HTTP/1.0 200 OK\r\nContent-Length: " 
                      `BL.append` (BL.pack . show $ messageLength) 
                      --`BL.append` BL.pack "\r\nContent-Type: plain/text"
                      `BL.append` BL.pack "\r\nContent-Encoding: gzip"
                      `BL.append` BL.pack "\r\n\r\n"
  | otherwise = BL.pack "HTTP/1.0 200 OK\r\nContent-Length: " 
                      `BL.append` (BL.pack . show $ messageLength) 
                      --`BL.append` BL.pack "\r\nContent-Type: plain/text"
                      `BL.append` BL.pack "\r\n\r\n"
--}

--subjectTxt="http://kilauea.bbspink.com/megami/subject.txt"
--datFileExample = "http://kilauea.bbspink.com/megami/dat/1325061495.dat"
--serverExample = "http://pele.bbspink.com/vault/erobbs/oyster/1285/1285357421.dat"

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
      | fromJust serverN `elem` serverStyle1 = urlStyle1
      | fromJust serverN `elem` serverStyle2 = urlStyle2
      | fromJust serverN `elem` serverStyle3 = urlStyle3
      | fromJust serverN `elem` serverStyle4 = urlStyle4
      | fromJust serverN `elem` serverStyle5 = urlStyle5
      | otherwise = urlStyle6

    urlStyle1 :: String
    urlStyle1 = mconcat ["http://", fromJust serverN, ".bbspink.com/vault/", fromJust boardN, "/", checkForPool, "/", fromJust postN, ".dat"]
    
    urlStyle2 :: String
    urlStyle2 = mconcat ["http://", fromJust serverN, ".bbspink.com/vault/", fromJust boardN, "/", postFirstFour, "/", postFirstFive, "/", fromJust postN, ".dat.gz"]

    urlStyle3 :: String
    urlStyle3 = mconcat ["http://", fromJust serverN, ".bbspink.com/vault/", fromJust boardN, "/kako/", postFirstFour, "/", postFirstFive, "/", fromJust postN, ".dat"]

    urlStyle4 :: String
    urlStyle4 = mconcat ["http://", fromJust serverN, ".bbspink.com/vault/", fromJust boardN, "/kako/", postFirstThree, "/", fromJust postN, ".dat"]

    urlStyle5 :: String
    urlStyle5 = mconcat ["http://", fromJust serverN, ".2ch.net/vault/_datArea/", fromJust boardN, "/", checkForPool, "/", fromJust postN, ".dat"]

    --http://banana3000.maido3.com/~ch2live20/vault/
    -- banana3000.maido3.com/<server>/vault/<board>/oyster/<first four>/<dat>.dat
    urlStyle6 :: String
    urlStyle6 = mconcat ["http://banana3000.maido3.com/", fromJust serverName2ch, "/vault/", fromJust boardN, "/", checkForPool, "/", fromJust postN, ".dat"]

    -- Check if pool or oyster. If it is oyster, then append the post's first four dat numbers.
    checkForPool 
      | poolOrOyster == "oyster" = mconcat [poolOrOyster, "/", postFirstFour]
      | otherwise = poolOrOyster 

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

    -- this was so long, its in the 2CHBOARDS.hs file
    serverStyle6 :: [String]
    serverStyle6 = serverStyle6'


-- Download the dat file from the server.
getMsg :: Input -> IO (BL.ByteString, Int)
getMsg input = do
  -- get the current time for authorization purposes
  getCurrentTime <- epochTime
  -- convert the current time to an integer
  let currentTime = read . show $ getCurrentTime :: Integer
  authenticationLogic currentTime
 where
  -- Make the url to download the dat file
  makeUrl :: String -> Maybe String
  makeUrl poolOrOyster = buildUrl poolOrOyster (server input) (board input) (post input)

  -- Check for errors, if no error, then download and return dat file
  authenticationLogic :: Integer -> IO (BL.ByteString, Int)
  authenticationLogic currentTime
    | anInputError = return inputError
    | not . fst $ authenticate currentTime = return authenticationError
    | isNothing (makeUrl "oyster") = return urlError
    | isNothing (makeUrl "pool") = return urlError
    | otherwise = do
        limitBool <- isUserOverDatLimit userHash -- check if user is over speedlimit or not
        if limitBool 
          then return timeLimitError -- if they are over the limit, return an error
          else downloadAndReturnDatFile  -- otherwise download and return dat file
   where
    -- Download the dat file and return it
    -- It first checks oyster. If oyster returns 404 not found, then it will check pool. If pool also returns 404 not found,
    -- then it prints "Error 13"
    downloadAndReturnDatFile :: IO (BL.ByteString, Int)
    downloadAndReturnDatFile = do
      print $ userHash ++ " is authenticated"-- user is authenticated, continue and download the page
      datFile <- getUrl oysterUrl
      addUserToTimer userHash -- add the user's hash to the dat timer
      if datFile == (fst pageDoesNotExistError) || datFileDoesNotExist datFile
        then do
          datFilePool <- getUrl poolUrl -- check if pool exists or not
          if datFilePool == (fst pageDoesNotExistError) || datFileDoesNotExist datFilePool -- if pool doesnt exist, just return the error
            then return pageDoesNotExistError -- this will return the error 13
            else return $ (mconcat [success, pool, n, processDatFile input datFilePool], 200) -- prepend success to pool
        else return $ (mconcat [success, oyster, n, processDatFile input datFile], 200) -- prepend success to oyster
     where
      success = BL.pack "Success"
      oyster = BL.pack " Oyster"
      n = BL.pack "\n"
      pool = BL.pack " Pool"
      statusExceptionHandler ::  HttpException -> IO BL.ByteString
      statusExceptionHandler e = (putStrLn "404 Not Found") >> toLog "error" (show currentTime ++ " 404") >> (return $ fst pageDoesNotExistError)
      oysterUrl = fromMaybe "" $ makeUrl "oyster"
      poolUrl = fromMaybe "" $ makeUrl "pool"
      getUrl url' = simpleHttp url' `X.catch` statusExceptionHandler

    userHash = snd . authenticate $ currentTime


  --processDatFile runs all the operations on the dat file for formatting
  -- dont forget to run the mosaic! (replaceDates)
  processDatFile input datFile = replaceDates ((startToEnd (start input) (end input) (keepFirst input) . lastN (Types.last input) (keepFirst input) $ datFile))
  -- Shows whether the user is authenticated or not. If authenticated, then the Bool is true, and string contains their hashed name
  -- if not authenticated then bool is false, and the string is just ""
  -- The integer sent to authenticate is the currentTime
  authenticate :: Integer -> (Bool, String)
  authenticate = authenticateUser input -- Crypto related

  -- Example:
  -- sid = Just "sid=Monazilla/2.00:437576303V875807Q65482S5373415V0353657X819589B683935C83892l0684065u718984C13042Y073615439W33071V8555402N76303M0122748U5567915F128809I381065V6928103Q47334M0251341Y65808j5567915e7"

  -- output an error if there is one
  anInputError :: Bool
  anInputError = fromMaybe False (error input)

-- Errors. 
inputError = (BL.pack "Error 8008135", 404)
authenticationError = (BL.pack "Error 69", 401)
urlError = (BL.pack "Error 666", 400)
timeLimitError = (BL.pack "Error 420", 401)
pageDoesNotExistError = (BL.pack "Error 13", 404)

{-- -- Alternative error messages
inputError = BL.pack "404 Not Found - Please check input and try again"
authenticationError = BL.pack "401 Unauthorized - Authentication Error"
urlError = BL.pack "400 Bad Request - Check URL and try again"
timeLimitError = BL.pack "401 Unauthorized - Request Limit Reached, please wait and try again"
--}

