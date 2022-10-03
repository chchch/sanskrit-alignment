module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO.CodePage (withCP65001)
import System.Environment (getArgs)
import System.Console.GetOpt (getOpt, OptDescr(Option), ArgOrder(Permute), ArgDescr(ReqArg))
import Data.Maybe (fromJust)
import Data.Align.Affine (allIndices, multiTrace)
--import Affine
import MyFasta (parseFasta')
import Collate (alignPrep, multiXMLAlign, alignLookup, recursiveLookup, simpleLookup, mtr, makeMatrix, makeArray, makePenalties)
import Filter (prepAksaras, prepSeqs, prepWords)

data Options = Options
    { optMatrixFile :: Maybe FilePath
    , optMatch :: Double
    , optMismatch :: Double
    , optGapOpen :: Double
    , optGap :: Double
    , optScript :: String
    , optLemma :: String
    }
    deriving Show

defaultOptions:: Options
defaultOptions = Options
    { optMatrixFile = Nothing
    , optMatch = 1
    , optMismatch = -1
    , optGapOpen = -3
    , optGap = -0.25
    , optScript = "iast"
    , optLemma = "character"
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['x']  ["matrix-file"] 
        (ReqArg (\f opts -> opts {optMatrixFile = Just f}) "MATRIXFILE")  "matrix file",
      Option ['M']  ["match"]      
        (ReqArg (\o opts -> opts {optMatch = read o::Double}) "MATCHSCORE") "match score",
      Option ['m']  ["mismatch"]   
        (ReqArg (\o opts -> opts {optMismatch = read o::Double}) "MISMATCHSCORE") "mismatch score",
      Option ['G']  ["gap-open"]        
        (ReqArg (\o opts -> opts {optGapOpen = read o::Double}) "GAPOPENSCORE") "gap opening score",
      Option ['g']  ["gap-extend"]        
        (ReqArg (\o opts -> opts {optGap = read o::Double}) "GAPEXTENDSCORE") "gap extension score",
      Option ['s']  ["script"]     
        (ReqArg (\s opts -> opts {optScript = s}) "SCRIPT") "script (iast or slp1)",
      Option ['l']  ["lemma"]      
        (ReqArg (\s opts -> opts {optLemma = s}) "LEMMA") "lemma size (character, aksara, word)"
      ]

parseArgs :: [String] -> IO(Options, String)
parseArgs argv = case getOpt Permute options argv of
    (args,strs,[]) -> do
        if length strs == 0
            then do ioError (userError "no filename specified")
            else if length strs /= 1
                then do ioError (userError "too many arguments")
                else return (foldl (flip id) defaultOptions args, head strs)
    (_,_,errs) -> do
        ioError (userError $ concat errs)

main :: IO ()
main = do
    setLocaleEncoding utf8
    (as, fname) <- getArgs >>= parseArgs
    f <- withCP65001 $ readFile fname
    let seqs = parseFasta' f
    let split 
            | optLemma as == "word"   = prepWords seqs
            | optLemma as == "aksara" = prepAksaras seqs
            | otherwise               = prepSeqs seqs

    let penalties = makePenalties (optMatch as) (optMismatch as) (optGapOpen as) (optGap as)
    let m = optMatrixFile as
    contents <- maybeReadFile m
    let lookupfn
            | contents == ""          = simpleLookup
            | optLemma as == "word"   = recursiveLookup mm
            | optLemma as == "aksara" = recursiveLookup mm
            | otherwise               = alignLookup mm
            where mm = makeMatrix . makeArray $ lines contents
    let result = mtr lookupfn penalties split
    withCP65001 $ putStrLn $ multiXMLAlign $ alignPrep (allIndices result) (multiTrace result) split

maybeReadFile:: Maybe FilePath -> IO (String)
maybeReadFile f
    | f == Nothing  = pure ""
    | otherwise     = withCP65001 $ readFile $ fromJust f
