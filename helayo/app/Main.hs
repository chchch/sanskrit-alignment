module Main where

import System.Environment
--import System.IO
import System.Console.GetOpt
--import System.Exit
import Data.Maybe
--import Data.Align
import Align
--import Data.Fasta.String.Parse
--import Data.Fasta.String.Types
--import Criterion.Main
--import qualified Data.Map as M
--import qualified Data.Matrix as X
import MyFasta
import Collate
import Transcribe
import Filter

data Options = Options
    {optMatrixFile :: Maybe FilePath,
     optMatch :: Double,
     optMismatch :: Double,
     optInitialGap :: Double,
     optGap :: Double,
     optScript :: String,
     optLemma :: String,
     optTest   :: Bool,
     optFile    :: Maybe FilePath
    }
    deriving Show

defaultOptions:: Options
defaultOptions = Options
    {optMatrixFile = Nothing,
     optMatch = 3,
     optMismatch = -3,
     optInitialGap = -3,
     optGap = -2,
     optScript = "iast",
     optLemma = "character",
     optTest = False,
     optFile = Nothing
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['x']  ["matrix-file"] (ReqArg (\f opts -> opts {optMatrixFile = Just f}) "FILE")  "matrix file",
      Option ['M']  ["match"]      (ReqArg (\o opts -> opts {optMatch = read o::Double}) "MATCHSCORE") "match score",
      Option ['m']  ["mismatch"]   (ReqArg (\o opts -> opts {optMismatch = read o::Double}) "MISMATCHSCORE") "mismatch score",
      Option ['i']  ["initial-gap"] (ReqArg (\o opts -> opts {optInitialGap = read o::Double}) "INITIALGAPSCORE") "initial gap score",
      Option ['g']  ["gap"]        (ReqArg (\o opts -> opts {optGap = read o::Double}) "GAPSCORE") "gap score",
      Option ['s']  ["script"]     (ReqArg (\s opts -> opts {optScript = s}) "SCRIPT") "script (iast or slp1)",
      Option ['l']  ["lemma"]      (ReqArg (\s opts -> opts {optLemma = s}) "LEMMA") "lemma size (character, aksara, word)",
      Option ['t']  ["test"]       (NoArg (\opts -> opts {optTest = True})) "test mode",
      Option ['f']  ["file"]       (ReqArg (\f opts -> opts {optFile = Just f}) "FILE") "fasta file"
      ]

parseArgs :: [String] -> IO(Options, [String])
parseArgs argv = case getOpt Permute options argv of
    (args,strs,[]) -> do
--        if length strs /= 2
--            then do exitWith (ExitFailure 1)
--           else return (foldl (flip id) defaultOptions args,strs)
        return (foldl (flip id) defaultOptions args, strs)
    (_,_,errs) -> do
        ioError (userError $ concat errs)
        --exitWith (ExitFailure 1)

main :: IO ()
main = do
    (as, fs) <- getArgs >>= parseArgs
    if optTest as == True then do
    {-
        -- testing stuff -- this doesn't work, ugh
        let penalties = makePenalties (optMatch as) (optMismatch as) (optInitialGap as) (optGap as)
        let mmatrix = mSM (M.fromList $ zip ["a","b","c","d","e","f"] [1..])
                          (M.fromList $ zip ["a","b","c","d","e","f"] [1..])
                          (X.fromLists $ repeat [1..5])
        let lmatrix = mSM' ["a","b","c","d","e","f"]
                           ["a","b","c","d","e","f"]
                           (repeat [1..5])
        defaultMain [
            bgroup "matrix" [ bench "a/b" $ nf (alignLookup mmatrix penalties "a") "b"
                ],
            bgroup "list" [ bench "a/b" $ nf (alignLookup' lmatrix penalties "a") "b"
                ]
    ]
    -}
        putStrLn $ "Flags: " ++ show as
        let mf = optMatrixFile as
        let transed
                | optScript as == "iast" = map (transliterateString iast slp1) fs
                | otherwise = fs
        let unfilters_strs = map (filterAll' filters) transed
        let strs = map (splitGlyphs slp1 . snd) unfilters_strs
        let unfiltered1 = unfilterAll' (fst $ head unfilters_strs) (head strs)
        let unfiltered2 = unfilterAll' (fst $ last unfilters_strs) (last strs)
        if mf /= Nothing then do
            contents <- readFile $ fromJust mf
            let mm = makeMatrix' . makeArray $ lines contents
            let penalties = makePenalties (optMatch as) (optMismatch as) (optInitialGap as) (optGap as)
            let result = tr mm penalties strs
            putStrLn $ "Strings: " ++ show fs
            print $ show result            
            putStrLn $ aksaraAlign (trace $ result)
        else do
    --    contents <- readFile fn
    --    let fl = lines contents
            putStrLn $ "Strings: " ++ show fs
            let penalties = makePenalties (optMatch as) (optMismatch as) (optInitialGap as) (optGap as)
            let result = tr' penalties strs
            print $ traceScore result
            putStrLn $ foldl (++) "" unfiltered1
            putStrLn $ foldl (++) "" unfiltered2

    else if optFile as /= Nothing then do
        f <- readFile (fromJust $ optFile as)
        let seqs = parseFasta' f
        let split 
                | optLemma as == "word"   = prepWords seqs
                | optLemma as == "aksara" = prepAksaras seqs
                | otherwise               = prepSeqs seqs
        let penalties = makePenalties (optMatch as) (optMismatch as) (optInitialGap as) (optGap as)
        let m = optMatrixFile as
        if m /= Nothing then do
            contents <- readFile $ fromJust m
            let mm = makeMatrix' . makeArray $ lines contents
            let result
                    | optLemma as == "word"   = mtrWords' mm penalties split
                    | optLemma as == "aksara" = mtrWords' mm penalties split
                    | otherwise               = mtr' mm penalties split
            putStrLn $ multiXMLAlign $ alignPrep (allIndices result) (multiTrace result) split-- make this take [String]s
        else do
            let result = mtr penalties split
            putStrLn $ multiXMLAlign $ alignPrep (allIndices result) (multiTrace result) split
    else do
        putStrLn $ "Flags: " ++ show as
        let mf = optMatrixFile as
        let transed
                | optScript as == "iast" = map (transliterateString iast slp1) fs
                | otherwise = fs
        let unfilters_strs = map (filterAll' filters) transed
        let strs = map (splitGlyphs slp1 . snd) unfilters_strs
        let unfiltered1 = unfilterAll' (fst $ head unfilters_strs) (head strs)
        let unfiltered2 = unfilterAll' (fst $ last unfilters_strs) (last strs)
        if mf /= Nothing then do
            contents <- readFile $ fromJust mf
            let mm = makeMatrix' . makeArray $ lines contents
            let penalties = makePenalties (optMatch as) (optMismatch as) (optInitialGap as) (optGap as)
            let result = tr mm penalties strs
            putStrLn $ "Strings: " ++ show fs
            print $ traceScore result            
            putStrLn $ aksaraAlign (trace $ result)
        else do
    --    contents <- readFile fn
    --    let fl = lines contents
            putStrLn $ "Strings: " ++ show fs
            let penalties = makePenalties (optMatch as) (optMismatch as) (optInitialGap as) (optGap as)
            let result = tr' penalties strs
            print $ traceScore result
            putStrLn $ foldl (++) "" unfiltered1
            putStrLn $ foldl (++) "" unfiltered2
            --putStrLn . aksaraAlign . trace $ result
            putStrLn $ aksaraAlign' (trace $ result) (map (transliterateString slp1 iast) unfiltered1) (map (transliterateString slp1 iast) unfiltered2)
