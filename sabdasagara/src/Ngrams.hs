module Ngrams
    --( ngram, ngram', ngram'',
    (  Grams, GramBag,
      Penalties (..),
      aksaraGrams, allGrams, reduceGrams,
      sortGrams, alignGrams, printGrams
    ) where

import Data.Maybe
import Data.List
import Data.Align
import Transcribe
import qualified Data.Vector as V
import qualified Data.Map as MM

type VString = V.Vector Char
type Grams = [VString]

data GramBag = GramBag {
    gSiglum :: String,
    gGrams :: [(Grams,String)]
    --gGrams :: [MM.Map Grams String]
    } deriving Show

data Penalties = Penalties {
    p1Gram :: Double,
    pNgram :: Double,
    pMatch :: Double,
    pMismatch :: Double,
    pGap :: Double,
    pGapX :: Double,
    pVowelVowel :: Double,
    pConsonantConsonant :: Double
    } deriving Show

aksaraGrams :: Int -> [(String,([String],[String]))] -> [GramBag]
aksaraGrams n xs = map go xs
    where
    go :: (String,([String],[String])) -> GramBag
    go (siglum,(sics,norms)) = GramBag {
            gSiglum = siglum,
            gGrams = ngram'' n (zip norms sics)
        }

allGrams :: [GramBag] -> [Grams]
allGrams xs = nub $ foldl (++) [] $ map go xs
    where
    go x = map fst $ gGrams x

reduceGrams:: [Grams] -> Penalties -> [[Grams]]
reduceGrams aa p = go [] $ map (\x -> [x]) aa
    where
    go:: [[Grams]] -> [[Grams]] -> [[Grams]]
    go done [] = reverse done
    go done (s:ss)
        -- | found == Nothing = go (done ++ [s]) ss
        | found == Nothing = go (s:done) ss
        | otherwise        = go done (fromJust found)
        where
        found = findSimilar s ss p

findSimilar:: [Grams] -> [[Grams]] -> Penalties -> Maybe [[Grams]]
findSimilar a bs p = go [] a bs
    where
    go:: [[Grams]] -> [Grams] -> [[Grams]] -> Maybe [[Grams]]
    go _ _ [] = Nothing
    go done x (y:ys)
        | scoreGrams x y p = Just (reverse done ++ (x ++ y):ys)
        | otherwise        = go (y:done) x ys

scoreGrams :: [Grams] -> [Grams] -> Penalties -> Bool
scoreGrams xs ys p = scoreall $ zip <$> xs <*> ys
    where
    -- e.g., zip <$> [["liṅ","ga"]] <*> [["liṃ","ge"],["liṃ","gye"]] ==
    --       [[("liṅ","liṃ"),("ga","ge")],[("liṅ","liṃ"),("ga","gye")]]
    scoreall :: [[(VString,VString)]] -> Bool
    scoreall [] = True
    scoreall (m:ms)
        | score 0 m == False = False
        | otherwise          = scoreall ms
        where
        -- e.g., [("liṅ","liṃ"),("ga","ge")]
        --       fst n == "liṅ", snd n == "liṃ"
        score :: Double -> [(VString,VString)] -> Bool
        score _ [] = True
        score mem (n:ns)
            | scoreone       < p1Gram p = False
            | cumulative     < pNgram p = False
            | otherwise                 = score cumulative ns
            where
            scoreone = traceScore $ align (gramConfig p) (fst n) (snd n)
            cumulative = mem + scoreone
        
gramConfig p = alignConfig lookup (pGap p) (pGapX p)
    where
    lookup :: Char -> Char -> Double
    lookup a b
        | a == b = pMatch p
        | isV a && isV b = pVowelVowel p
        | isC a && isC b = pConsonantConsonant p
        | otherwise      = pMismatch p
        where
        isV x = x `elem` "aAiIuUfFxXeEoO"
        isC x = x `elem` "kKgGNcCjJYwWqQRtTdDnpPbBmyrlvSzshL"

sortGrams :: [GramBag] -> [(String,MM.Map Grams String)]
sortGrams xs = map sortGram xs
    where
    sortGram :: GramBag -> (String,MM.Map Grams String)
    sortGram x = (gSiglum x, MM.fromListWith (\a b -> a ++ ';':b) $ gGrams x)

alignGrams :: [(String,MM.Map Grams String)] -> [[Grams]] -> [(String,[String])]
alignGrams xs headers = map go xs
    where
    go :: (String,MM.Map Grams String) -> (String,[String])
    go (siglum,grams) = (siglum, map gogo headers)
        where
        gogo :: [Grams] -> String
        gogo header = maybeIntercalate mapped
            where
            mapped = map (\h -> fromMaybe "" $ MM.lookup h grams) header
            maybeIntercalate xs
                | foldl (&&) True $ map null xs = ""
                | otherwise                     = intercalate ";;" xs

printGrams :: (String,[String]) -> String
printGrams (sig,grams) = sig ++ ',' : (intercalate "," $ map go grams)
    where
    go :: String -> String
    go x = '"':(transliterateString slp1' iast x) ++ "\""

{-
ngram :: Int -> [a] -> [[a]]
ngram n xs
    | n <= length xs = take n xs : ngram n (drop 1 xs)
    | otherwise      = []

ngram' :: Int -> [(String,String)] -> [(String,String)]
ngram' n xs
    | n <= length xs = go (take n xs) : ngram' n (drop 1 xs)
    | otherwise      = []
    where
    go :: [(String,String)] -> (String,String)
    go = foldl (\(a,b) (c,d) -> (a++c,b++d)) ("","")
-}

ngram'' :: Int -> [(String,String)] -> [(Grams,String)]
ngram'' n xs
    | n <= length xs = (rev . collect $ take n xs) : ngram'' n (drop 1 xs)
    | otherwise      = []
    where
    rev :: (Grams,String) -> (Grams,String)
    rev (m,n) = (reverse m,n)

    collect :: [(String,String)] -> (Grams,String)
    collect = foldl (\(a,b) (c,d) -> (V.fromList c:a,b++d)) ([],"")
