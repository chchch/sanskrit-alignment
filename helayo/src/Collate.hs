module Collate (
--tr,
mtr,
makeArray,
SubstMatrix,--SubstMatrix',
--mSM,mSM',
makeMatrix,--makeMatrix',
Penalties,
makePenalties,
--aksaraAlign,aksaraAlign',
--multiStrAlign,multiStrAlign',
multiXMLAlign,
alignPrep,
alignLookup,recursiveLookup,simpleLookup
) where

import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector as V (fromList)
import Data.Align.Affine (alignConfig, align, stepOfAll, trace, traceScore, centerStar, MultiStep, MultiTrace)
--import Affine
import Data.List (transpose, foldl')
import Data.List.Split (wordsBy)
import qualified Data.Map as M (lookup, fromList, Map)
import qualified Data.Matrix as X (Matrix, fromLists, (!))
import Transcribe (splitGlyphs, slp1', slp1'2iast, iast2slp1', transliterateString)

data Penalties = Penalties {
    pMatch :: Double,
    pMismatch :: Double,
    pGapOpen :: Double,
    pGap :: Double
} deriving (Show)

makePenalties:: Double -- match score
             -> Double -- mismatch score
             -> Double -- gap opening score
             -> Double -- gap extension score
             -> Penalties
makePenalties = Penalties

alignLookup:: SubstMatrix -> Penalties -> String -> String -> Double
alignLookup m o a b
    | a == b                        = pMatch o
    | (xindex >> yindex) == Nothing = pMismatch o
    | otherwise                     = matrix m X.! (fromJust xindex,fromJust yindex)
        where
        xindex = a `M.lookup` rows m
        yindex = b `M.lookup` columns m

recursiveLookup :: SubstMatrix -> Penalties -> String -> String -> Double
recursiveLookup m o a b
    | a == b                        = pMatch o
    | otherwise                     = (traceScore t) / fromIntegral (length $ trace t)
    where
        t = align (alignConfig (alignLookup m o) (pGapOpen o) (pGap o)) 
                      (V.fromList a') (V.fromList b')
        a' = splitGlyphs slp1' a
        b' = splitGlyphs slp1' b
{-
alignLookup' :: SubstMatrix' -> Penalties -> String -> String -> Double
alignLookup' m o a b
    | a == b                        = pMatch o
    | (xindex >> yindex) == Nothing = pMismatch o
    | otherwise                     = (matrix' m !! (fromJust xindex)) !! (fromJust yindex)
        where
        xindex = a `elemIndex` rows' m
        yindex = b `elemIndex` columns' m
-}

simpleLookup :: Penalties -> String -> String -> Double
simpleLookup o a b = if a == b then pMatch o else pMismatch o

{-
tr m conf o ss = align
       (alignConfig (conf o) (pGapOpen o) (pGap o))
       (V.fromList $ head ss)
       (V.fromList $ last ss)
-}

mtr :: (Penalties -> String -> String -> Double) -> Penalties -> [(String,([String],[String]))] 
                -> MultiTrace String String Double
mtr conf o seqs = centerStar
    (alignConfig (conf o) (pGapOpen o) (pGap o))
    vecs
    where
    vecs = map (\(i,(_,s)) -> (i,V.fromList s)) seqs

makeArray :: [String] -> [[String]]
makeArray = map (wordsBy (== ';'))


data SubstMatrix = SubstMatrix {columns :: M.Map String Int,
                      rows :: M.Map String Int,
                      matrix :: X.Matrix Double} deriving (Show)
{-
data SubstMatrix' = SubstMatrix' {columns' :: [String],
                      rows' :: [String],
                      matrix' :: [[Double]]} deriving (Show)

mSM' :: [String] -> [String] -> [[Double]] -> SubstMatrix'
mSM' c r m = SubstMatrix' {columns' = c, rows' = r, matrix' = m}

mSM :: M.Map String Int -> M.Map String Int -> X.Matrix Double -> SubstMatrix
mSM c r m = SubstMatrix {columns = c, rows = r, matrix = m}


makeMatrix' :: [[String]] -> SubstMatrix
makeMatrix' (ss:sss) = SubstMatrix {columns = xaxis, rows = yaxis, matrix = scores}
    where
    xaxis = M.fromList $ zip ss [1..]
    yaxis = M.fromList $ zip (map head sss) [1..]
    --size = length xaxis * length yaxis - 1
    scores = X.fromLists $ map (map (\s -> read s::Double)) $ map (drop 1) sss
-}

-- Matrix is 1-indexed
makeMatrix :: [[String]] -> SubstMatrix
makeMatrix sses = SubstMatrix {columns = xaxis, rows = yaxis, matrix = scores}
    where
    ss = head sses
    sss = tail sses
    xaxis = M.fromList $ zip (map (transliterateString iast2slp1') ss) [1..]
    yaxis = M.fromList $ zip (map (transliterateString iast2slp1' . head) sss) [1..]
    --size = length xaxis * length yaxis - 1
    scores = X.fromLists $ map (map (\s -> read s::Double)) $ map (drop 1) sss

{-        
aksaraAlign :: [Step String] -> String
aksaraAlign = go id id
    where
    go as bs [] = as "\n" ++ bs ""
    go as bs (t:ts) = case t of
        Left (Left c)   -> go (as . (c ++)) (bs . (take (length c) (repeat '-') ++)) ts
        Left (Right c)  -> go (as . (take (length c) (repeat '-') ++)) (bs . (c ++)) ts
        Right (c,d)     -> go (as . (c ++) . (fill1 ++)) (bs . (d ++) . (fill2 ++)) ts
            where
            fill1 = take (length d - length c) (repeat '*')
            fill2 = take (length c - length d) (repeat '*')

aksaraAlign' :: [Step String] -> [String] -> [String] -> String
aksaraAlign' ss a b = go id id ss a b
    where
    go :: (String -> String) -> (String -> String) -> [Step String] -> [String] -> [String] -> String
    go as bs [] _ _ = as "\n" ++ bs ""
    go as bs _ [] _ = as "\n" ++ bs ""
    go as bs _ _ [] = as "\n" ++ bs ""
    go as bs (t:ts) ss1@(s1:rem1) ss2@(s2:rem2) = case t of
        Left (Left c)   -> go (as . (s1 ++) . (","++)) (bs . (take (length s1) (repeat '-') ++) . (","++)) ts rem1 ss2
        Left (Right c)  -> go (as . (take (length s2) (repeat '-') ++) . (","++)) (bs . (s2 ++) . (","++)) ts ss1 rem2
        Right (c,d)     -> go (as . (s1 ++) . (fill1 ++) . (","++)) (bs . (s2 ++) . (fill2 ++) . (","++)) ts rem1 rem2
            where
            fill1 = take (length s2 - length s1) (repeat '*')
            fill2 = take (length s1 - length s2) (repeat '*')

multiStrAlign :: [MultiStep String] -> String
multiStrAlign = unlines . map (\y -> foldr (\x acc -> x ++ "," ++ acc) "" (map (fromMaybe "") y)) . transpose . map stepOfAll
-}

alignPrep :: [String] -> [MultiStep String] -> [(String,([String],[String]))] -> [(String,([String],[Maybe String]))]
alignPrep is ms ss = reverse $ go (M.fromList (zip is (transpose $ map stepOfAll ms))) ss []
    where 
    go :: M.Map String [Maybe String] -> [(String,([String],[String]))] -> [(String,([String],[Maybe String]))] -> [(String,([String],[Maybe String]))]
    go _ [] acc = acc
    go m ((i,(unfiltered,_)):rm) acc = go m rm ((i,(unfiltered,target)):acc)
        where target = fromMaybe [] (M.lookup i m)
{-    
alignZip' :: [String] -> [Maybe String] -> [String]
alignZip' a b = go a b id
    where
    go :: [String] -> [Maybe String] -> ([String] -> [String]) -> [String]
    go _ [] acc = acc []
    go [] ms acc = acc $ map (fromMaybe "") ms
    go ss@(s:srem) (m:mrem) acc
        | m == Nothing  = go ss mrem (acc . ([""]++))
        | otherwise     = go srem mrem (acc . ([s]++))
-}
alignZip :: [String] -> [Maybe String] -> [(String,String)]
alignZip a b = go a b []
    where
    go :: [String] -> [Maybe String] -> [(String,String)] -> [(String,String)]
    go _ [] acc = reverse acc
    go [] ms acc = go [""] ms acc
    go ss@(s:srem) (m:mrem) acc
        | m == Nothing       = go ss mrem (("",""):acc)
        | otherwise          = go srem mrem ((s,which):acc)
            where 
            which
                | s == fromJust m    = ""
                | otherwise = fromJust m

{-
multiStrAlign' :: [(String,([String],[Maybe String]))] -> String
multiStrAlign' as = foldl' (\x acc -> x ++ "\n" ++ acc) "" (map go as)
    where
    go :: (String,([String],[Maybe String])) -> String
    go (i,(s,m)) = i ++ "," ++ result
        where 
        result = foldr (\x acc -> (transliterateString slp1' iast x) ++ "," ++ acc) "" $ alignZip s m
-}

multiXMLAlign :: [(String,([String],[Maybe String]))] -> String
multiXMLAlign as = 
    header ++
    (foldl' (\x acc -> x ++ "\n" ++ acc) "" $ map formatTEIText as) ++
    footer
    where
    header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<teiCorpus xmlns=\"http://www.tei-c.org/ns/1.0\">\n"
    footer = "\n</teiCorpus>"
    formatTEIText :: (String,([String],[Maybe String])) -> String
    formatTEIText (i,(s,m)) = "<TEI n=\"" ++ i ++ "\">\n<text>\n" ++ snd result ++ "\n</text></TEI>"
        where 
        result = foldr go (length m,"") $ alignZip s m
            where 
            go :: (String,String) -> (Int,String) -> (Int,String)
            go x acc =
                (num, 
                    "<w n=\"" ++ show num ++ lemma ++ "\">" ++ 
                    (transliterateString slp1'2iast $ fst x) ++ "</w>" ++ 
                    snd acc
                )
                where 
                num = fst acc - 1
                lemma
                    | snd x /= "" = "\" lemma=\"" ++ (transliterateString slp1'2iast (snd x))
                    | otherwise   = "" 
