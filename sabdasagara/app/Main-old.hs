import System.Environment (getArgs)
import System.IO (readFile)
import System.Console.GetOpt
import System.Exit
import Data.Ord (comparing)
import Data.Align
import Data.Maybe
import Data.List
import Data.List.Split
import Data.String.Utils as S
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Map as MM
import Transcribe
import Filter
import Stem
import MyFasta

data Options = Options
    {optPenalty :: Double,
     optStopwords :: String
    }
    deriving Show

defaultOptions = Options
    {optPenalty = 1,
     optStopwords = ""
    }

options :: [OptDescr (Options -> Options)]
options =
    [
    Option ['s']  ["stopwords"]       (ReqArg (\s opts -> opts {optStopwords = s}) "STOPWORDS") "stopwords",
    Option ['p']  ["penality"]        (ReqArg (\o opts -> opts {optPenalty = read o::Double}) "PENALTY") "penalty"
  ]


parseArgs argv = case getOpt Permute options argv of
    (args,strs,[]) -> do
        return (foldl (flip id) defaultOptions args, strs)
    (_,_,errs) -> do
        exitWith (ExitFailure 1)

main :: IO ()
main = do
    (as, fs) <- getArgs >>= parseArgs
    
    s <- readFile $ head fs
    let sws = optStopwords as
    {-
    let bag = bagInBag' $ sortBy (comparing length) $ bow (addStopWords sws) (transliterateString iast slp1' $ S.strip s)
    --putStrLn $ intercalate "," bag
    --putStrLn $ M.prettyMatrix $ M.fromLists . scoreMatrix $ bagInBag' bag
    mapM_ putStrLn (prettyPrint bag (scoreMatrix bag))
-}
    let seqs = parseFasta' s
    let split = prepWords' seqs
    -- split is [(siglum, ([unnormalized],[normalized]))]
    doc <- readFile "SL_stems.csv"
    doc2 <- readFile "more_stems.csv"
    let normsplit = stemmit split (doc++doc2)
    -- normsplit is [(siglum, ([unnormalized],[[stems]]))]
    --let norms = snd $ snd $ head normsplit
    --let normcat = map (intercalate ",") norms
    --mapM_ putStrLn normcat

    --let stems = reducestems $ listallstems normsplit
    --mapM_ putStrLn $ map (intercalate ",") stems
    
    let stems = sort $ reducestems $ listallstems normsplit
    let ts = sortBags normsplit stems
    putStrLn $ transliterateString slp1' iast $ ',':(intercalate "," $ map (intercalate " ") stems)
    mapM_ putStrLn $ printBags ts (length stems)

    --let bag = bow (addStopWords sws) (transliterateString iast slp1' $ S.strip s)
    
    --doc <- readFile "SL_stems.csv"
    --mapM_ putStrLn $ sortBy (comparing length) $ nub $ doStem bag doc

listallstems:: [(a,(b,[[String]]))] -> [[String]]
listallstems ss = foldl go [] ss
    where
    go:: [[String]] -> (a,(b,[[String]])) -> [[String]]
    go done (_,(_,todo)) = done ++ todo

reducestems:: [[String]] -> [[String]]
reducestems aa = go [] aa
    where 
    go:: [[String]] -> [[String]] -> [[String]]
    go done [] = done
    go done (s:ss)
        | found == Nothing = go (done ++ [s]) ss
        | otherwise   = go done (fromJust found)
        where
        found = findInter s ss

findInter:: [String] -> [[String]] -> Maybe [[String]]
findInter a bs = go [] a bs
    where
    go:: [[String]] -> [String] -> [[String]] -> Maybe [[String]]
    go _ _ [] = Nothing
    go done x (y:ys)
        | intersect x y /= [] = Just (done ++ (union x y):ys)
        | otherwise           = go (done ++ [y]) x ys

stemmit:: [(String,([String],[String]))] -> String -> [(String,([String],[[String]]))]
stemmit s doc = map go s
    where 
    go:: (String,([String],[String])) -> (String,([String],[[String]]))
    go (a,(b,c)) = (a,(b,doStem' c doc))
   
sortBags:: [(String,([String],[[String]]))] -> [[String]] -> [(String,MM.Map Int String)]
sortBags ss stems = map sortBag ss
    where
    sortBag:: (String,([String],[[String]])) -> (String,MM.Map Int String)
    sortBag (sig,(unnorm,norm)) = (sig, MM.fromListWith (++) $ go [] zipped)
        where
        zipped = zip norm unnorm
        stemmap = zip stems [1..]
        go:: [(Int, String)] -> [([String],String)] -> [(Int,String)]
        go done [] = done
        go done (s:ss) = go ((pos,snd s):done) ss
            where
            pos = fromJust (findPos (fst s) stemmap)

findPos:: [String] -> [([String],Int)] -> Maybe Int
findPos _ [] = Nothing
findPos tofind (z:zs)
    | intersect tofind (fst z) /= [] = Just (snd z)
    | otherwise                = findPos tofind zs

printBags:: [(String,MM.Map Int String)] -> Int -> [String]
printBags bags max = map printBag bags
    where
    printBag:: (String,MM.Map Int String) -> String
    printBag (sig,lem) = sig ++ ',':(go counters)
        where
        counters = [1..max]
        go:: [Int] -> String
        go [] = ""
        go (i:is)
            | found == Nothing = ',':(go is)
            | otherwise        = ('"':(gogo found) ++ "\"") ++ ',':(go is)
            where
            gogo x = transliterateString slp1' iast $ fromJust x
            found = MM.lookup i lem


stopwords = ["ca","vā","hi",
             "iva","eva","iti",
             "ādayaḥ",
             "yaḥ","saḥ","asau",
             "yat","tat","etat",
             "anena","yena","tena",
             "yasmai","tasmai","kasmai",
             "asya","yasya","tasya",
             "ataḥ","yataḥ","tataḥ",
             "yayoḥ","tayoḥ",
             "atra","yatra","tatra",
             "yathā","tathā"]

addStopWords :: String -> [String]
addStopWords "" = stopwords
addStopWords s = stopwords ++ (splitOn "," s)

uniq' :: [String] -> [String] -> [String]
uniq' sws [] = []
uniq' sws (x:xs)
    | x `elem` sws = uniq' sws xs
    | x `elem` xs = uniq' sws xs
    | otherwise = x:(uniq' sws xs)
{-
uniquish :: [[String]] -> [[String]]
uniquish [] = []
uniquish (x:xs) = go [] x xs
    where
    go as x bs 
    | fst $ score x as 
-}

maxScore::Double
maxScore = 0.3

type VString = V.Vector Char

aConfig = alignConfig (\a b -> if a == b then 1.5 else (-0.25::Double)) (-0.5) (-1)

-- returns lowest scored match and index of match
score :: VString -> V.Vector [VString] -> (Double,Int)
score x yys = V.ifoldl go (maxScore,0) yys
    where
    go :: (Double,Int) -> Int -> [VString] -> (Double, Int)
    go (oldsc,oldn) curn ys
        | newsc < oldsc = (newsc,curn)
        | otherwise = (oldsc,oldn)
        where
        newsc = foldl (\sc y -> minimum [sc,lowsc y]) oldsc ys
            where
            lowsc = traceScore . (align aConfig x)

score' :: VString -> [VString] -> [Double]
score' x ys = map go ys
    where
    go y = (traceScore $ align aConfig x y) / maxlen
        where
        maxlen = fromIntegral $ max (V.length x) (V.length y)

scoreMatrix :: [VString] -> [[Double]]
scoreMatrix xs = go [] start xs
    where
    start = take (length xs) (repeat [])
    go :: [[Double]] -> [[Double]] -> [VString] -> [[Double]]
    go done _ [] = done
    go done (p:ps) (t:ts) = go (done ++ [p ++ 0:scoredrow]) scoredcol ts
        where
        scoredrow = score' t ts
        scoredcol = zipWith (++) ps (map (\x -> [x]) scoredrow)
        --scoredcol = getZipList $ (++) <$> ZipList ps <*> ZipList (map (\x -> [x]) scoredrow)


prettyPrint :: [VString] -> [[Double]] -> [String]
prettyPrint xs yys = map (intercalate ",") $ ["":zs] ++ (zipWith (:) zs ystrs)
--prettyPrint xs yys = map (intercalate ",") $ ["":zs] ++ (getZipList $ (++) <$> ZipList (map (\x -> [x]) zs) <*> ZipList ystrs)
    where
    ystrs = map (map show) yys
    zs = map V.toList xs

-- |Produces a bag of words, starting with a list of stopwords and a string.
bow :: [String] -> String -> [String]
bow sws s = go sws (splitOneOf " -+" s)
    where 
    go :: [String] -> [String] -> [String]
    go sws ss = uniq' sws' ss
      where
      sws' :: [String]
      sws' = map (transliterateString iast slp1') sws

-- |Prepares the bag of words for analysis.
bagInBag :: [String] -> V.Vector [VString]
bagInBag = V.fromList . map (\x -> [x]) . map V.fromList . sortBy (comparing length)

bagInBag' :: [String] -> [VString]
bagInBag' = map V.fromList

shakeBag :: V.Vector [VString] -> V.Vector [VString]
shakeBag xxs = go (V.empty,xxs)
    where
    go :: (V.Vector [VString],V.Vector [VString]) -> V.Vector [VString]
    go (done,todo)
        | next == Nothing = done V.++ todo
        | otherwise = go $ go2 done todo
        where
        findNext :: V.Vector [VString] -> Maybe Int
        findNext xxs = V.findIndex (\xs -> length xs == 1) xxs
        next = findNext todo
        go2 :: V.Vector [VString] -> V.Vector [VString] -> (V.Vector [VString],V.Vector[VString])
        go2 a b = V.splitAt (fromJust next) todo
