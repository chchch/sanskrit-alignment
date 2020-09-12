module Transcribe (
ScriptScheme,
TransScheme,
iast,
slp1,
fasta,
transliterateString,
splitGlyphs) where

import Data.Maybe
import Data.Array
import Data.List
import Data.Ord
import qualified Data.Map as Map


data ScriptScheme = ScriptScheme { vowels :: [String],
                                   consonants :: [String],
                                   marks :: [String] } deriving (Show)

iast = ScriptScheme {vowels = ["a","ā","i","ī","u","ū","ṛ","ṝ","ḷ","ḹ","ẽ","e","ai","õ","o","au"],
                     consonants = ["k","kh","g","gh","ṅ","c","ch","j","jh","ñ","ṭ","ṭh","ḍ","ḍh","ṇ","t","th","d","dh","n","p","ph","b","bh","m","y","r","l","v","ś","ṣ","s","h","ḻ","ḻh"],
                     marks = ["ṃ","ḥ","m̐","oṁ","oḿ"]}

--deva = ScriptScheme {vowels = ["अ","आ","इ","ई","उ","ऊ","ए","ऐ","ओ","औ"],
--                     consonants = ["क","ख","ग","घ","ङ"],
--                     marks = []}

slp1 = ScriptScheme {vowels = ["a","A","i","I","u","U","f","F","x","X","3","e","E","0","o","O"],
                     consonants = ["k","K","g","G","N","c","C","j","J","Y","w","W","q","Q","R","t","T","d","D","n","p","P","b","B","m","y","r","l","v","S","z","s","h","L","|"],
                     marks = ["M","H","~","'","`"]}

-- SLP1 with variations for use with bioinformatics software
fasta = ScriptScheme {vowels = ["a","A","i","I","u","U","f","F","x","X","3","e","E","0","o","O"],
                     consonants = ["k","K","g","G","N","c","C","j","J","Y","w","W","q","Q","R","t","T","d","D","n","p","P","b","B","m","y","r","l","v","S","z","s","h","L","1"],
                     marks = ["M","H","2","4","6"]}

data TransScheme = TransScheme { fromScheme :: ScriptScheme,
                                 toScheme :: ScriptScheme,
                                 charKeys :: [String],
                                 charMap :: Map.Map String String,
                                 maxLength :: Int} deriving (Show)

makeTransScheme:: ScriptScheme -> ScriptScheme -> TransScheme
makeTransScheme a b = TransScheme {fromScheme = a,
                                   toScheme = b,
                                   charKeys = allChars a,
                                   charMap = mapChars a b,
                                   maxLength = findLongest $ allChars a}
    where
        allChars s = (consonants s) ++ (vowels s) ++ (marks s)
        mapChars:: ScriptScheme -> ScriptScheme -> Map.Map String String
        mapChars a b = Map.fromList $ zip (allChars a) (allChars b)
        findLongest:: [String] -> Int
        findLongest ss = length $ maximumBy (comparing length) ss

matchGlyph :: String -> TransScheme -> Int -> (String,String)
matchGlyph s ts 0 = ([head s],tail s) -- if nothing matches, leave the first character as is
matchGlyph s ts n
    | pre `elem` (charKeys ts) = (transliterate pre $ charMap ts,post)
    | otherwise = matchGlyph s ts (n-1)
    where
        pre = take n s
        post = drop n s
        transliterate x tm = fromMaybe "" (Map.lookup x tm)

splitGlyphs :: String -> ScriptScheme -> [String]
splitGlyphs s sc = reverse $ splitLoop [] s
    where
        charList = (consonants sc) ++ (vowels sc) ++ (marks sc)
        maxLen = length $ maximumBy (comparing length) charList
        splitLoop :: [String] -> String -> [String]
        splitLoop ss ""  = ss
        splitLoop ss rem = splitLoop (fst result:ss) (snd result)
            where
                result = findChar rem charList maxLen
                    where
                        findChar :: String -> [String] -> Int -> (String,String)
                        findChar s2 cl 0 = ([head s2],tail s2)
                        findChar s2 cl n
                            | pre `elem` cl = (pre,post)
                            | otherwise = findChar s2 cl (n-1)
                            where
                                pre = take n s2
                                post = drop n s2

transliterateString :: String -> ScriptScheme -> ScriptScheme -> String
transliterateString s sc1 sc2 = transliterateLoop "" s ts
    where
        ts = makeTransScheme sc1 sc2
        transliterateLoop :: String -> String -> TransScheme -> String
        transliterateLoop s1 "" _  = s1
        transliterateLoop s1 s2 ts = transliterateLoop (s1 ++ fst result) (snd result) ts
            where
                maxLen = maxLength ts
                result = matchGlyph s2 ts maxLen

