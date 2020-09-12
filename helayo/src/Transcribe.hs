module Transcribe (
ScriptScheme,
TransScheme,
iast,
slp1,
slp1',
fasta,
transliterateString,
splitGlyphs, splitGlyphs_,
splitAksaras
) where

import Data.Maybe
import Data.Array
import Data.List
import Data.Ord
import qualified Data.Map as Map


data ScriptScheme = ScriptScheme { vowels :: [String],
                                   consonants :: [String],
                                   marks :: [String] } deriving (Show)

iast = ScriptScheme {vowels = ["a","ā","i","ī","u","ū","ṛ","ṝ","ḷ","ḹ","ẽ","e","ai","õ","o","au","ê","ô","aî","aû"],
                     consonants = ["k","kh","g","gh","ṅ","c","ch","j","jh","ñ","ṭ","ṭh","ḍ","ḍh","ṇ","t","th","d","dh","n","p","ph","b","bh","m","y","r","l","v","ś","ṣ","s","h","ḻ","ḻh","ṙ"],
                     marks = ["ṃ","ḥ","m̐","oṁ","oḿ"]}

--deva = ScriptScheme {vowels = ["अ","आ","इ","ई","उ","ऊ","ए","ऐ","ओ","औ"],
--                     consonants = ["क","ख","ग","घ","ङ"],
--                     marks = []}

slp1 = ScriptScheme {vowels = ["a","A","i","I","u","U","f","F","x","X","3","e","E","0","o","O"],
                     consonants = ["k","K","g","G","N","c","C","j","J","Y","w","W","q","Q","R","t","T","d","D","n","p","P","b","B","m","y","r","l","v","S","z","s","h","L","|"],
                     marks = ["M","H","~","'","`"]}

slp1' = ScriptScheme {vowels = ["a","A","i","I","u","U","f","F","x","X","ee","e","E","oo","o","O","ê","ô","aî","aû"],
                     consonants = ["k","K","g","G","N","c","C","j","J","Y","w","W","q","Q","R","t","T","d","D","n","p","P","b","B","m","y","r","l","v","S","z","s","h","L","LL","ṙ"],
                     marks = ["M","H","m̐","oṁ","oḿ"]}

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

--- splits out spaces as separate field
splitGlyphs_ :: ScriptScheme -> String -> [String]
splitGlyphs_ sc = reverse . splitLoop []
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

--- includes trailing spaces with each character
splitGlyphs :: ScriptScheme -> String -> [String]
splitGlyphs sc = reverse . splitLoop []
    where
    charList = (consonants sc) ++ (vowels sc) ++ (marks sc)
    maxLen = length $ maximumBy (comparing length) charList
    splitLoop :: [String] -> String -> [String]
    splitLoop ss ""  = ss
    splitLoop ss rem = splitLoop (fst result':ss) (snd result')
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
        result' = gobbleSpaces result
            where
            gobbleSpaces :: (String,String) -> (String,String)
            gobbleSpaces (pre,post)
                | post == []       = (pre,post)
                | otherwise        = (pre ++ spaces,other)
                    where (spaces,other) = span (== ' ') post
                -- | head post == ' ' = gobbleSpaces (pre ++ " ",drop 1 post)
                -- | otherwise        = (pre,post)

data CharType = Consonant | Vowel | Mark | Other | Space
    deriving (Eq)

getType :: String -> ScriptScheme -> CharType
getType s sc
    | s == " "                 = Space
    | s `elem` (vowels sc)     = Vowel
    | s `elem` (consonants sc) = Consonant
    | s `elem` (marks sc)      = Mark
    | otherwise                = Other

splitAksaras :: ScriptScheme -> String -> [String]
splitAksaras sc ss = reverse $ go [] Space chars
    where
    chars = splitGlyphs_ sc ss
    go :: [String] -> CharType -> [String] -> [String]
    go mems _ [] = mems
    go [] _ (ch:chs) = go [ch] (getType ch sc) chs
    go mems@(mem:rems) cache (ch:chs)
        | curType == Space                           = splitOff curType
        | curType == Vowel && cache /= Consonant     = splitOff curType
        | curType == Consonant && cache /= Consonant = splitOff curType
        | curType == Mark && cache /= Vowel          = splitOff curType
        | otherwise                                  = addOn curType
        where 
            curType = getType ch sc
            splitOff t = go (ch:mems) t chs
            addOn t = go ((mem++ch):rems) t chs

transliterateString :: ScriptScheme -> ScriptScheme -> String -> String
transliterateString sc1 sc2 = transliterateLoop ts id
    where
    ts = makeTransScheme sc1 sc2
    transliterateLoop :: TransScheme -> (String -> String) -> String -> String
    transliterateLoop _ s1 ""  = s1 ""
    transliterateLoop ts s1 s2 = transliterateLoop ts (s1 . (fst result ++)) (snd result)
        where
        maxLen = maxLength ts
        result = matchGlyph s2 ts maxLen

transliterateString' :: ScriptScheme -> ScriptScheme -> String -> [String]
transliterateString' sc1 sc2 = transliterateLoop ts id
    where
    ts = makeTransScheme sc1 sc2
    transliterateLoop :: TransScheme -> ([String] -> [String]) -> String -> [String]
    transliterateLoop _ s1 ""  = s1 []
    transliterateLoop ts s1 s2 = transliterateLoop ts (s1 . ([fst result] ++)) (snd result)
        where
        maxLen = maxLength ts
        result = matchGlyph s2 ts maxLen

