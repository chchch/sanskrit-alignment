module Transcribe (
ScriptScheme,
TransScheme,
iast,slp1',
iast2slp1',
slp1'2iast,
transliterateString,
splitGlyphs, splitGlyphs_,
splitAksaras
) where

import Data.Maybe (fromMaybe)
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Map as Map (Map, fromList, lookup)


data ScriptScheme = ScriptScheme { vowels :: [String]
                                 , consonants :: [String]
                                 , marks :: [String]
                                 , ssAll :: [String]
                                 , ssLength :: Int
                                   } deriving (Show)

makeScriptScheme:: [String] -> [String] -> [String] -> ScriptScheme
makeScriptScheme v c m =
    let vcm = v ++ c ++ m
    in ScriptScheme { vowels = v
                    , consonants = c
                    , marks = m
                    , ssAll = vcm
                    , ssLength = findLongest vcm
                    }

findLongest:: [String] -> Int
findLongest ss = length $ maximumBy (comparing length) ss

iast = makeScriptScheme 
    ["a","ā","i","ī","u","ū","ṛ","ṝ","ḷ","ḹ","ẽ","e","ai","õ","o","au","ê","ô","aî","aû"]
    ["k","kh","g","gh","ṅ","c","ch","j","jh","ñ","ṭ","ṭh","ḍ","ḍh","ṇ","t","th","d","dh","n","p","ph","b","bh","m","y","r","l","v","ś","ṣ","s","h","ḻ","ḻh","ṙ"]
    ["ṃ","ḥ","m̐","oṁ","oḿ"]

slp1 = makeScriptScheme
    ["a","A","i","I","u","U","f","F","x","X","3","e","E","0","o","O"]
    ["k","K","g","G","N","c","C","j","J","Y","w","W","q","Q","R","t","T","d","D","n","p","P","b","B","m","y","r","l","v","S","z","s","h","L","|"]
    ["M","H","~","'","`"]

slp1' = makeScriptScheme
    ["a","A","i","I","u","U","f","F","x","X","ẽ","e","E","õ","o","O","ê","ô","aî","aû"]
    ["k","K","g","G","N","c","C","j","J","Y","w","W","q","Q","R","t","T","d","D","n","p","P","b","B","m","y","r","l","v","S","z","s","h","L","LL","ṙ"]
    ["M","H","m̐","oṁ","oḿ"]

-- SLP1 with variations for use with bioinformatics software
fasta = makeScriptScheme
    ["a","A","i","I","u","U","f","F","x","X","3","e","E","0","o","O"]
    ["k","K","g","G","N","c","C","j","J","Y","w","W","q","Q","R","t","T","d","D","n","p","P","b","B","m","y","r","l","v","S","z","s","h","L","1"]
    ["M","H","2","4","6"]

data TransScheme = TransScheme { fromScheme :: ScriptScheme
                               , toScheme :: ScriptScheme
                               , charKeys :: [String]
                               , charMap :: Map.Map String String
                               , tsLength :: Int
                               } deriving (Show)

makeTransScheme:: ScriptScheme -> ScriptScheme -> TransScheme
makeTransScheme a b = TransScheme { fromScheme = a
                                  , toScheme = b
                                  , charKeys = ssAll a
                                  , charMap = mapChars a b
                                  , tsLength = ssLength a
                                  }
    where
    mapChars:: ScriptScheme -> ScriptScheme -> Map.Map String String
    mapChars a b = Map.fromList $ zip (ssAll a) (ssAll b)

iast2slp1' = makeTransScheme iast slp1'
slp1'2iast = makeTransScheme slp1' iast

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
    splitLoop :: [String] -> String -> [String]
    splitLoop ss ""  = ss
    splitLoop ss rem = splitLoop (fst result:ss) (snd result)
        where
        result = findChar rem (ssAll sc) (ssLength sc)

--- includes trailing spaces with each character
splitGlyphs :: ScriptScheme -> String -> [String]
splitGlyphs sc = reverse . splitLoop []
    where
    splitLoop :: [String] -> String -> [String]
    splitLoop ss ""  = ss
    splitLoop ss rem = splitLoop (fst result:ss) (snd result)
        where
        result = gobbleSpaces $ findChar rem (ssAll sc) (ssLength sc)
            where
            gobbleSpaces :: (String,String) -> (String,String)
            gobbleSpaces (pre,post)
                | post == []       = (pre,post)
                | otherwise        = (pre ++ spaces,other)
                    where (spaces,other) = span (== ' ') post

findChar :: String -> [String] -> Int -> (String,String)
findChar s2 cl 0 = ([head s2],tail s2)
findChar s2 cl n
    | pre `elem` cl = (pre,post)
    | otherwise = findChar s2 cl (n-1)
    where
    pre = take n s2
    post = drop n s2

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

transliterateString :: TransScheme -> String -> String
transliterateString ts = transliterateLoop id
    where
    transliterateLoop :: (String -> String) -> String -> String
    transliterateLoop s1 ""  = s1 ""
    transliterateLoop s1 s2 = transliterateLoop (s1 . (fst result ++)) (snd result)
        where
        maxLen = tsLength ts
        result = matchGlyph s2 ts maxLen

{-
transliterateString' :: ScriptScheme -> ScriptScheme -> String -> [String]
transliterateString' sc1 sc2 = transliterateLoop ts id
    where
    ts = makeTransScheme sc1 sc2
    transliterateLoop :: TransScheme -> ([String] -> [String]) -> String -> [String]
    transliterateLoop _ s1 ""  = s1 []
    transliterateLoop ts s1 s2 = transliterateLoop ts (s1 . ([fst result] ++)) (snd result)
        where
        maxLen = tsLength ts
        result = matchGlyph s2 ts maxLen
-}
