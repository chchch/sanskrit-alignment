module Stem
    ( doStem, doStem'
    ) where

import qualified Data.Map as M
import Data.List.Split
import Data.List
import Data.Maybe
import Control.Monad

doStem :: [String] -> String -> [String]
doStem xs doc =
    map (\x -> fromMaybe x $ M.lookup x m) xs
    where m = stemMap doc

doStem' :: [String] -> String -> [[String]]
doStem' xs doc =
    map (\x -> fromMaybe [x] $ M.lookup x m) xs
    where m = stemMap' doc

stemMap:: String -> M.Map String String
stemMap = M.fromList . map (toTwo . splitOn ",") . lines

stemMap':: String -> M.Map String [String]
stemMap' = M.map nub . M.fromListWith (++) . map (toTwo' . splitOn ",") . lines

toTwo :: [String] -> (String,String)
toTwo [x,y] = (x,y)

toTwo' :: [String] -> (String,[String])
toTwo' [x,y] = (x,[y])
