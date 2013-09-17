module ChanBoards where

import Data.Set
import qualified Data.Map as M

serverStyle6' :: [String]
serverStyle6' = [""]

serverMap :: M.Map String String
serverMap = M.fromList [("","")]


listOfServers :: Set String
listOfServers = fromList ["babiru","qiufen","idol","set","venus","yomi","sakura01","peach","sakura02","pie","kilauea","pele","okazu","wow","pink","www2"] -- with vip removed until gzip is fixed
--listOfServers = ["babiru","qiufen","idol","set","venus","yomi","sakura01","peach","sakura02","pie","kilauea","pele","vip","okazu","wow","pink","www2"] -- full list

listOfBoards :: Set String
listOfBoards = fromList ["kitchen","21overkanto","21overkita","21overnishi","hnews","pinkqa","sureh","erolive","hneta","pinkcafe","eromog2","ogefin","pinknanmin","21oversea2","hgame","hgame2","erog","leaf","adultsite","webmaster","avideo","avideo2","nude","eroanime","erocomic","erodoujin","natuero","kgirls","erocosp","eroacademy","mcheck","couple","kageki","kageki2","onatech","loveho","adultgoods","adultaccessory","sm","feti","mature","okama","gaypink","lesbian","eroaa","erochara","erochara2","801","erocg","eroparo","ascii","ascii2d","ascii2kana","girls","sportgirls","club","pub","host","nuki","soap","neet4pink","cherryboy","megami","gagame","801saloon","bbbb","meow","mobpink","ccc","housekeeping","erobbs","yama"]

