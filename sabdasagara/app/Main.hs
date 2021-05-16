import System.Environment (getArgs)
import System.IO (readFile)
import System.Console.GetOpt
import System.Exit
import Data.List
import qualified Data.Vector as V
--import Data.Ord (comparing)
--import Data.Align
--import Data.Maybe
--import Data.List.Split
--import Data.String.Utils as S
--import qualified Data.Matrix as M
--import qualified Data.Map as MM
import Transcribe
import Filter
import MyFasta
import Ngrams

data Options = Options
    {optFuzzy :: Bool,
     optGrams :: Int,
     optP1Gram :: Double,
     optPNGram :: Double,
     optPMatch :: Double,
     optPMismatch :: Double,
     optPGap :: Double,
     optPGapX :: Double,
     optPVowelVowel :: Double,
     optPConsonantConsonant :: Double
    }
    deriving Show

defaultOptions = Options
    {optFuzzy = False,
      optGrams = 3,
      optP1Gram = (-1),
      optPNGram = (-1),
      optPMatch = (0),
      optPMismatch = (-2),
      optPGap = (-1.125),
      optPGapX = (-1.25),
      optPVowelVowel = (-1),
      optPConsonantConsonant = (-2)
    }

options :: [OptDescr (Options -> Options)]
options =
    [
    Option ['n']  ["ngrams"]    (ReqArg (\n opts -> opts {optGrams = read n::Int}) "2") "number of ngrams",
    Option ['f']  ["fuzzy"]     (NoArg (\opts -> opts {optFuzzy = True})) "turn on fuzzy matching",
    Option ['p']  ["1gram-penalty"]  (ReqArg (\o opts -> opts {optP1Gram = read o::Double}) "-1") "1gram fuzzy match minimum score",
    Option ['q']  ["ngram-penalty"]  (ReqArg (\o opts -> opts {optPNGram = read o::Double}) "-1") "ngram fuzzy match minimum score"
  ]


parseArgs argv = case getOpt Permute options argv of
    (_,[],_) -> ioError (userError (usageInfo header options))
        where header = "Usage: sabdasagara [OPTION...] file.fas"
    (args,strs,[]) -> do
        return (foldl (flip id) defaultOptions args, strs)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: sabdasagara [OPTION...] file.fas"
    --(_,_,errs) -> do
    --    exitWith (ExitFailure 1)

main :: IO ()
main = do
    (as, fs) <- getArgs >>= parseArgs
    
    s <- readFile $ head fs
    let seqs = parseFasta' s
    let ps = Penalties {
        p1Gram = optP1Gram as,
        pNgram = optPNGram as,
        pMatch = optPMatch as,
        pMismatch = optPMismatch as,
        pGap = optPGap as,
        pGapX = optPGapX as,
        pVowelVowel = optPVowelVowel as,
        pConsonantConsonant = optPConsonantConsonant as
    }
     
    let split = prepAksaras seqs
    let ngrammed = aksaraGrams (optGrams as) split
    let allgrams = allGrams ngrammed
    let reducedgrams = if optFuzzy as == True 
        then reduceGrams allgrams ps
        else map (\x -> [x]) allgrams
    putStrLn $  ',' :
        (intercalate "," $ 
            map (\xs -> quotate $ intercalate ";;" $ 
                map (transliterateString slp1' iast . gramsToString) xs) reducedgrams)
    let aligned = alignGrams (sortGrams ngrammed) reducedgrams
    mapM_ putStrLn $ map printGrams aligned

quotate :: String -> String
quotate s = '"':s ++ "\""

gramsToString :: Grams -> String
gramsToString = concat . map (V.toList)

{-
type VString = V.Vector Char

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

maxScore::Double
maxScore = 0.3

--type VString = V.Vector Char

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
-}
