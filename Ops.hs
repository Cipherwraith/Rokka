module Ops where

import qualified Data.ByteString.Lazy as BL hiding (pack)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Maybe

-- Test file is a simulation of what the data will be like directly from 2ch/bbspink
--  example:
--    "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30\n31\n32\n33\n34\n35\n36\n37\n38\n39\n40\n41\n42\n43\n44\n45\n46\n47\n48\n49\n50\n51\n52\n53\n54\n55\n56\n57\n58\n59\n60\n61\n62\n63\n64\n65\n66\n67\n68\n69\n70\n71\n72\n73\n74\n75\n76\n77\n78\n79\n80\n81\n82\n83\n84\n85\n86\n87\n88\n89\n90\n91\n92\n93\n94\n95\n96\n97\n98\n99\n100"
testFile :: BL.ByteString
testFile = BL.intercalate (BL.pack "\n") . map BL.pack . map show $ [1..100]

-- lastN takes the last 'n' amount of posts in a dat file, and returns them
lastN :: Maybe Int -> Maybe Bool -> BL.ByteString -> BL.ByteString
lastN n b s = if isNothing n 
               then s
               else if (fromJust n <= 0) -- make sure the amount to take is greater than 0
                    then s
                    -- if B is nothing or true, then return with post 1, else remove it
                    else if (isNothing b || fromJust b == True) 
                      -- add first post
                      then if (fromJust n) >= lengthThread -- if n is bigger than the thread, just output thread
                        then s
                        else firstPost `BL.append` BL.pack "\n" `BL.append` lastPosts 
                      -- dont add the first post
                      else lastPosts 
  where
    lastPosts = BL.unlines . reverse . take (fromJust n) . reverse . BL.lines $ s
    lengthThread = length . BL.lines $ s
    firstPost = takeN 1 s

-- takeN takes one specific reply line in a dat file, and returns it
takeN :: Int -> BL.ByteString -> BL.ByteString
-- check if the reply wanted is less than 0 or greater than the total file size
takeN n s = if n <= 0 || (length splitS) < n
              then s -- If n is out of range, just return the whole string
              else splitS !! (n - 1) -- Else return the single line
  where
    splitS = BL.lines s

-- startTo takes an int, and copies 
startToEnd :: Maybe Int -> Maybe Int -> Maybe Bool -> BL.ByteString -> BL.ByteString
startToEnd maybeStart maybeEnd maybeKeepFirstPost fileContent
  | start > end       = fileContent
  | start < 0         = fileContent
  | end < 0           = fileContent
  | start == end      = takeN start fileContent
  | otherwise         = takeBetween
 where
  -- set start to 0 if its Nothing
  start = fromMaybe 0 maybeStart
  -- set end to the total length of the post if its Nothing
  end = fromMaybe (length linedPost) maybeEnd
  -- Keep the first post by default, unless maybeKeepFirstPost says otherwise
  keepFirstPost = fromMaybe True maybeKeepFirstPost
  -- if start is greater than 1, then we need to check if we have to prepend the first post or not.
  takeBetween :: BL.ByteString
  takeBetween = if start > 1 
                  then if keepFirstPost
                    -- we keep the first post by prepending it to the between posts
                    then takeN 1 fileContent `BL.append` BL.pack "\n" `BL.append` getBetweenPosts fileContent
                    else getBetweenPosts fileContent -- we dont keep the first post
                  else getBetweenPosts fileContent -- just be normal
  -- Get the posts between start and end, including both start and end
  getBetweenPosts :: BL.ByteString -> BL.ByteString
  getBetweenPosts x = BL.unlines . reverse . drop (length linedPost - end) . reverse . drop (start - 1) $ linedPost

  -- Split the thread into lines based on reply
  linedPost :: [BL.ByteString]
  linedPost = BL.lines fileContent
  

