module Download where

import Prelude hiding (error)

import Network.HTTP.Conduit

import Control.Exception as X

import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL


import Data.Monoid
import Data.Maybe

import System.Posix.Time

import GetHeaders
import BuildUrl
import Errors
import Types
import Utility
import Ops
import Auth
import DatTimer
import Mosaic
import Search

-- Download the dat file from the server.
getMsg :: Input -> IO (BL.ByteString, Int, String)
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
  authenticationLogic :: Integer -> IO (BL.ByteString, Int, String)
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
    downloadAndReturnDatFile :: IO (BL.ByteString, Int, String)
    downloadAndReturnDatFile = do
      -- print $ userHash ++ " is authenticated"-- user is authenticated, continue and download the page -- Debugging
      datFile <- getUrl oysterUrl
      addUserToTimer userHash -- add the user's hash to the dat timer
      if datFile == (fst' pageDoesNotExistError) || datFileExistence datFile
        then do
          datFilePool <- getUrl poolUrl -- check if pool exists or not
          if datFilePool == (fst' pageDoesNotExistError) || datFileExistence datFilePool -- if pool doesnt exist, just return the error
            then return pageDoesNotExistError -- this will return the error 13
            else do
              lModified <- lastModified poolUrl
              return $ (mconcat [success, pool, n, processDatFile input datFilePool], 200, lModified) -- prepend success to pool
        else do
            lModified <- lastModified oysterUrl
            return $ (mconcat [success, oyster, n, processDatFile input datFile], 200, lModified) -- prepend success to oyster
     where
      success = BL.pack "Success"
      oyster = BL.pack " Archive"
      n = BL.pack "\n"
      pool = BL.pack " Pool"
      live = BL.pack " Live"
      statusExceptionHandler ::  HttpException -> IO BL.ByteString
      statusExceptionHandler e = (putStrLn "404 Not Found") >> {-- toLog "error" (show currentTime ++ " 404") >> --} (return $ fst' pageDoesNotExistError)
      oysterUrl = fromMaybe "" $ makeUrl "oyster"
      poolUrl = fromMaybe "" $ makeUrl "pool"
      getUrl url' = simpleHttp url' `X.catch` statusExceptionHandler
      lastModified url' = getLastModified url'

    userHash = snd . authenticate $ currentTime


  --processDatFile runs all the operations on the dat file for formatting
  -- dont forget to run the mosaic! (replaceDates)
  processDatFile input datFile = replaceDates ((startToEnd (start input) (end input) (keepFirst input) . lastN (Types.last input) (keepFirst input) $ datFile))
  -- Shows whether the user is authenticated or not. If authenticated, then the Bool is true, and string contains their hashed name
  -- if not authenticated then bool is false, and the string is just ""
  -- The integer sent to authenticate is the currentTime
  authenticate :: Integer -> (Bool, String)
  authenticate =  authenticateUser input -- Crypto related

  -- Example:
  -- sid = Just "sid=Monazilla/2.00:437576303V875807Q65482S5373415V0353657X819589B683935C83892l0684065u718984C13042Y073615439W33071V8555402N76303M0122748U5567915F128809I381065V6928103Q47334M0251341Y65808j5567915e7"

  -- output an error if there is one
  anInputError :: Bool
  anInputError = fromMaybe False (error input)

