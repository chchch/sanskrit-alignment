import System.Environment
import System.Random
import Data.String.Unicode
import Transcribe
import Filter

import Data.List
import Data.Char
import Data.Array
import Text.Regex.PCRE
import Data.Either

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    mb <- readFile "test/mahabhasya.txt"
    rand <- randomIO :: IO Int
    let len = 300
    let strlen = length mb
    offs <- sequence $ replicate 10 $ randomRIO (0, strlen - len)
    let tests = map (\x -> filterTest x len mb) offs
    defaultMain (testGroup "Filter tests" tests)

substr off n = (take n) . (drop off)

filterTest start len str = 
    let startstr = substr start len str
        filtered = runFilter startstr
        unfiltered = concat $ fst $ runUnfilter filtered
        finalstr = xmlToUnicode $ transliterateString slp1'2iast unfiltered
    in testCase startstr (assertEqual "Should be equal" startstr finalstr)

xmlToUnicode s = replaceAll "&#(\\d+);" (\mt -> [chr $ strToInt (fst $ mt ! 1)]) s

strToInt s = read s :: Int

runFilter = (filterAll' (filters ++ [spaceFilter]) ) . unicodeToXmlEntity . (transliterateString iast2slp1')

runUnfilter (fs,s) = let split = splitGlyphs slp1' s in (unfilterAll' fs split, split)
