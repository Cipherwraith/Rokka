-- Name:          Rokka server
-- Author:        CipherWraith
-- Contact:       twitter.com/codemonkeyz
-- Last modified: Thu Sep 26 14:35:14 2013

module Main where

import Prelude hiding (error)
import Network
import Control.Concurrent
import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Monoid
import Data.Maybe
import System.Random
import System.IO
import System.Posix.Time
import Gzip
import Header
import Types
import Crypt
import DatTimer
import Logger
import Parse
import Utility
import Download
import Errors

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 80
  print "Welcome to the inside of the vault"
  mapM_ checkPaths directoriesToCheck
  loop sock

loop sock = do
  (h,x,z) <- accept sock
  currTime <- epochTime
  rand <- randomRIO (1000000,9999999) :: IO Int
  _ <- forkIO $ toLog "ip" $ mconcat [show currTime, " ", encryptT x rand]
  -- process one line at a time
  hSetBuffering h LineBuffering
  _ <- forkIO $ body h
  loop sock

-- Main logic loop
body h = do
  -- Get the incoming header
  headerGot <- hGetContents h :: IO String

  -- get the $_GET data and put it into a data type
  let getInput = parseHeaderStream headerGot :: HeaderNew
  currTime <- epochTime

  _ <- forkIO $ toLog "ua" $ mconcat [show currTime, " ", fromMaybe "" (userAgent getInput)]

  -- Parse the $_GET data, and figure out which board/post/server/sid the user is using
  let input = parseInput getInput :: Input
  
  -- Create the outgoing message with input 
  (msg, code, lastModified) <- getMsg input :: IO (BL.ByteString, Int, String)

  -- Check if the input asks for raw=0.0. If it does, then gzip the message
  -- returns True if the message is zipped, and False if not. Message is in the snd
  let possiblyCompressedMessage = checkForGZip getInput msg :: (Bool, BL.ByteString)

  -- Check if the message is zipped or not
  let isItZipped = fst possiblyCompressedMessage

  -- Get the message into a new variable
  let msg' = snd possiblyCompressedMessage

  -- Get the length of the message:
  let messageLength = BL.length msg' 

  -- Build the outgoing header
  let outgoingHeader = buildHeader code isItZipped messageLength lastModified :: BL.ByteString

  -- Output the data to the user
  -- Check if it is HEAD or not. If head, then dont output msg'.

  BL.hPut h . mconcat $ prepOut getInput code outgoingHeader msg'

  -- Clear the handle, and close it
  hFlush h
  hClose h
    where
      -- Gets the header sent by the user
      incomingHeader = BL.pack . getData. parseHeader 

-- prepOut checks if the header is POST, HEAD, or GET, then outputs accordingly
prepOut :: HeaderNew -> Int -> BL.ByteString -> BL.ByteString -> [BL.ByteString]
prepOut getInput code outgoingHeader msg'
  | httpMethod == "HEAD" = [outgoingHeader] -- HEAD: just return header
  | httpMethod == "GET" = [outgoingHeader, msg'] -- GET: return header and message
  | httpMethod `elem` methods = [error405NotAllowed] -- All other methods: output a 405 error here!
  | code == 404 = [outgoingHeader, msg']
  | code == 401 = [outgoingHeader, msg']
  | code == 400 = [outgoingHeader, msg']
  | otherwise = [error404NotFound] -- If no method, then return 404 not found
 where
  httpMethod = fst . requestQuery $ getInput

