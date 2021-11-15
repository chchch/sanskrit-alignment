module Main where

import System.Environment
import System.Console.GetOpt
import Data.Maybe
import Align
import MyFasta
import Collate
import Filter

data Options = Options
    {optMatrixFile :: Maybe FilePath,
     optMatch :: Double,
     optMismatch :: Double,
     optInitialGap :: Double,
     optGap :: Double,
     optGapOpen :: Double,
     optScript :: String,
     optLemma :: String
    }
    deriving Show

defaultOptions:: Options
defaultOptions = Options
    {optMatrixFile = Nothing,
     optMatch = 0.5,
     optMismatch = -1,
     optInitialGap = -0.25,
     optGap = -0.25,
     optGapOpen = -3,
     optScript = "iast",
     optLemma = "character"
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['x']  ["matrix-file"] (ReqArg (\f opts -> opts {optMatrixFile = Just f}) "FILE")  "matrix file",
      Option ['M']  ["match"]      (ReqArg (\o opts -> opts {optMatch = read o::Double}) "MATCHSCORE") "match score",
      Option ['m']  ["mismatch"]   (ReqArg (\o opts -> opts {optMismatch = read o::Double}) "MISMATCHSCORE") "mismatch score",
      Option ['i']  ["initial-gap"] (ReqArg (\o opts -> opts {optInitialGap = read o::Double}) "INITIALGAPSCORE") "initial gap score",
      Option ['g']  ["gap"]        (ReqArg (\o opts -> opts {optGap = read o::Double}) "GAPSCORE") "gap score",
      Option ['o']  ["gap-open"]        (ReqArg (\o opts -> opts {optGapOpen = read o::Double}) "GAPOPENSCORE") "gap opening score",
      Option ['s']  ["script"]     (ReqArg (\s opts -> opts {optScript = s}) "SCRIPT") "script (iast or slp1)",
      Option ['l']  ["lemma"]      (ReqArg (\s opts -> opts {optLemma = s}) "LEMMA") "lemma size (character, aksara, word)"
      ]

parseArgs :: [String] -> IO(Options, String)
parseArgs argv = case getOpt Permute options argv of
    (args,strs,[]) -> do
        if length strs /= 1
            then do ioError (userError "no filename specified")
            else return (foldl (flip id) defaultOptions args, head strs)
    (_,_,errs) -> do
        ioError (userError $ concat errs)

main :: IO ()
main = do
    (as, fname) <- getArgs >>= parseArgs
    f <- readFile fname
    let seqs = parseFasta' f
    let split 
            | optLemma as == "word"   = prepWords seqs
            | optLemma as == "aksara" = prepAksaras seqs
            | otherwise               = prepSeqs seqs
    let penalties = makePenalties (optMatch as) (optMismatch as) (optInitialGap as) (optGap as) (optGapOpen as)
    let m = optMatrixFile as
    if m /= Nothing then do
        contents <- readFile $ fromJust m
        let mm = makeMatrix . makeArray $ lines contents
        let result
                | optLemma as == "word"   = mtr (recursiveLookup mm) penalties split
                | optLemma as == "aksara" = mtr (recursiveLookup mm) penalties split
                | otherwise               = mtr (alignLookup mm) penalties split
        putStrLn $ multiXMLAlign $ alignPrep (allIndices result) (multiTrace result) split-- make this take [String]s
    else do
        let result = mtr simpleLookup penalties split
        putStrLn $ multiXMLAlign $ alignPrep (allIndices result) (multiTrace result) split
