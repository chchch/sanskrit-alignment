module Filter (
Filter,Filtered,
filters,
replaceAll,replaceAll',
unreplaceAll,
filterAll,filterAll',filterAll'',
unfilterAll,unfilterAll',
prepSeqs, prepWords, prepAksaras,
filterSearch,
filterReplace
)
where

import Data.List
import Data.Char
import Data.Array
import Text.Regex.PCRE
import Data.String.Unicode
import qualified Data.List.Split as S
import Data.Fasta.String.Types
import Transcribe

type ReplaceFn = MatchText String -> String

type Filtered = (ReplaceFn, [MatchText String])

data Filter = Filter
    {filterSearch :: String,
     filterReplace :: ReplaceFn,
     filterDesc :: String
    }

filters = [
    Filter {
        filterDesc = "valapalagilaka",
        filterSearch = "&#7769;",
        filterReplace = (const "r")
    },
    Filter {
        filterDesc = "pṛṣṭhamātrā e",
        filterSearch = "&#234;",
        filterReplace = (const "e")
    },
    Filter {
        filterDesc = "pṛṣṭhamātrā o",
        filterSearch = "&#244;",
        filterReplace = (const "o")
    },
    Filter {
        filterDesc = "pṛṣṭhamātrā ai",
        filterSearch = "a&#238;",
        filterReplace = (const "E")
    },
    Filter {
        filterDesc = "pṛṣṭhamātrā au",
        filterSearch = "a&#251;",
        filterReplace = (const "O")
    },
    Filter {
        filterDesc = "candrabindu",
        filterSearch = "m&#784;",
        filterReplace = (const "M")
    },
    Filter {
        filterDesc = "oṃkāras",
        filterSearch = "o&#774[35];",
        filterReplace = (const "oM")
    },
    Filter {
        filterDesc = "non-ASCII characters",
        filterSearch ="&#\\d+;",
        filterReplace = (const "")
    },
    Filter {
        filterDesc = "additional punctuation",
        filterSearch = "['()\\[\\],;'|_\\-=\\d'.\"\\\\/]+",
        filterReplace = (const "")
    },
    Filter {
        filterSearch = "(?:kK|gG|cC|jJ|wW|qQ|tT|dD|pP|bB)",
        filterReplace = (\mt -> tail . fst $ mt ! 0),
        filterDesc = "geminated aspirated consonants"
    },
    Filter {
        filterDesc = "geminated m after h",
        filterSearch = "(?:Mhm|hmm)",
        filterReplace = (const "hm")
    },
    Filter {
        filterDesc = "geminated t",
        filterSearch = "(?<=[rfi]|p[aA])tt|tt(?=[rvy]\\S)",
        filterReplace = (const "t")
    },
    Filter { 
        filterSearch = "(?<=[rf]|[rf]\\s)([kgcjwqdpbRnmyvl])\\1{1,2}", 
        filterReplace = (\mt -> fst $ mt ! 1),
        filterDesc = "geminated consonants after r"
    },
    Filter {
        filterDesc = "final nasal variants",
        filterSearch = "(?:M[lSs]|nn)(?!\\S)",
        filterReplace = (const "n")
    },
    Filter {
        filterDesc = "internal nasal variants",
        filterSearch = "[mnNYR](?=[pPbBmdDtTnwWqQcCjJkKgG])",
        filterReplace = (const "M")
    },
    Filter {
        filterDesc = "visarga āḥ variants",
        filterSearch = "AH(?=\\s[aAiIeEuUogGjJqQdDbBnmyrlvh])",
        filterReplace = (const "A")
    },
    Filter {
        filterDesc = "final anusvāra variants", -- A 8.4.59
        filterSearch = "M?[mN](?!\\S)|(?<=k[ai])n(?=\\st)|Y(?=\\s[jc])",
        filterReplace = (const "M")
    }, 
    Filter {
        filterDesc = "visarga aḥ before voiced consonants",
        filterSearch = "(?<!\\sB)(?:a[Hr]|[o])(?=\\s[gGjJqQdDnbBmrylvh])", -- ignore bho
        filterReplace = (const "aH")
    },
    Filter {
        filterDesc = "visarga aḥ before vowels",
        filterSearch = "aH(?=\\s[AiIeuUof])",
        filterReplace = (const "a")
    },
    Filter {
        filterDesc = "other visarga variants",
        filterSearch = "H?[rszS](?!\\S)",
        filterReplace = (const "H")
    },
    Filter {
        filterDesc = "internal visarga variants",
        --filterSearch = "(?<=u)z|z(?=k)|s(?=s)",
        filterSearch = "z(?=k)|s(?=s)",
        filterReplace = (const "H")
    },
    Filter {
        filterDesc = "final au/āv",
        filterSearch = "āv(?!\\S)",
        filterReplace = (const "au")
    },
    Filter {
        filterDesc = "kcch/kś",
        filterSearch = "k\\s*(?:S|c?C)",
        filterReplace = (const "kS")
    },
    Filter {
        filterDesc = "cch/ch/cś/tś",
        filterSearch = "c\\s*[CS]|t\\sS",
        filterReplace = (const "C")
    },
    Filter {
        filterDesc = "final t + voiced syllable", -- different rule for t + h = ddh
        filterSearch = "d(?=(?:\\s[aAiIeuUogGdDbByrv]|\\s*$))",
        filterReplace = (const "t")
    },
    Filter {
        filterDesc = "final t + n/m",
        filterSearch = "t(?=\\s[nm])",
        filterReplace = (const "n")
    },
    Filter {
        filterDesc = "final t + c/j",
        filterSearch = "j(?=\\sj)|c(?=\\sc)",
        filterReplace = (const "t")
    },
{-    Filter {    
        filterDesc = "i/y + vowel",
        filterSearch = "y(?=\\s[aAuUeo])",
        filterReplace = (const "i")
    },
-}
    Filter {
        filterDesc = "bhd for bdh",
        filterSearch = "Bd",
        filterReplace = (const "bD")
    }
    ]

spaceFilter' = 
    Filter {
        filterDesc = "collapse spaces",
        filterSearch = "\\s+",
        filterReplace = (const " ")
    }

spaceFilter =
    Filter {
        filterDesc = "spaces",
        filterSearch = "\\s+",
        filterReplace = (const "")
    }


-----
--
-- these functions are String -> String
--
-----

replaceAll :: String -> ReplaceFn -> String -> String
replaceAll re f s = start end
    where 
    (_, end, start) = foldl' go (0, s, id) ((s =~ re) :: [MatchText String])
    go (ind,read,write) mt =
        let (off,len) = snd $ mt ! 0 -- MatchText is Array [(String,(Int,Int))]; 0 is full match, followed by submatches
            (skip, start) = splitAt (off - ind) read 
            (_, remaining) = splitAt len start 
        in (off + len, remaining, write . (skip++) . (f mt ++))

filterAll :: [Filter] -> String -> String
filterAll [] s = s
filterAll (x:xs) s = filterAll xs (replaceAll (filterSearch x) (filterReplace x) s)

unfilterAll :: [Filtered] -> String -> String
unfilterAll [] s = s
unfilterAll (x:xs) s = unfilterAll xs (unreplaceAll x s)

unreplaceAll :: Filtered -> String -> String
unreplaceAll (_,[]) s = s -- when there were no replacements made
unreplaceAll (f,ms) s = start end
    where
    (_, end, start) = foldl' go (0, s, id) ms
    go (ind,read,write) m =
        let (txt,(off,origlen)) = m ! 0 -- MatchText is Array [(String,(Int,Int))]; 0 is full match, followed by submatches
            len = length $ f m
            (skip, start) = splitAt (off - ind) read 
            (_, remaining) = splitAt len start 
        in  (off + origlen, remaining, write . (skip++) . (txt++))

-------
--
-- these functions are String -> [String]
--
-------

-- outputs a list of (siglum,(unnormalized,normalized))
prepSeqs :: [FastaSequence] -> [(String,([String],[String]))]
prepSeqs ss = zip sigla unfiltered
    where 
    sigla = map fastaHeader ss
    filtered = map (filterAll' (filters ++ [spaceFilter']) . unicodeToXmlEntity . transliterateString iast slp1' . fastaSeq) ss
    unfiltered = map go filtered
        where go (fs,s) = let split = splitGlyphs_ slp1' s in (unfilterAll' fs split,split)

prepWords :: [FastaSequence] -> [(String,([String],[String]))]
prepWords ss = zip sigla unfiltered
    where 
    sigla = map fastaHeader ss
    filtered = map (filterAll' (filters ++ [spaceFilter']) . unicodeToXmlEntity . transliterateString iast slp1' . fastaSeq) ss
    unfiltered = map go filtered
        where go (fs,s) = let split = S.split (S.condense . S.keepDelimsR $ S.oneOf " ") s in (unfilterAll' fs split,split)

prepAksaras :: [FastaSequence] -> [(String,([String],[String]))]
prepAksaras ss = zip sigla unfiltered
    where 
    sigla = map fastaHeader ss
    filtered = map (filterAll' (filters ++ [spaceFilter]) . unicodeToXmlEntity . transliterateString iast slp1' . fastaSeq) ss
    unfiltered = map go filtered
        where go (fs,s) = let split = splitAksaras slp1' s in (unfilterAll' fs split,split)

replaceAll' :: String -> ReplaceFn -> String -> (Filtered,String)
replaceAll' re f s = ((f,ms),start end)
    where
    ms = ((s =~ re) :: [MatchText String])
    (_, end, start) = foldl' go (0, s, id) ms
    go (ind,read,write) m =
        let (off,len) = snd $ m ! 0 -- MatchText is Array [(String,(Int,Int))]; 0 is full match, followed by submatches
            (skip, start) = splitAt (off - ind) read 
            (_, remaining) = splitAt len start 
        in (off + len, remaining, write . (skip++) . (f m ++))

unreplaceAll' :: Filtered -> [String] -> [String]
unreplaceAll' (_,[]) ss = ss -- when there were no replacements made
unreplaceAll' (f,ms) ss = start end
    where
    (_, end, start) = foldl' go (0, ss, id) ms
    go (ind,read,write) m =
        let (txt,(off,_)) = m ! 0 -- MatchText is Array [(String,(Int,Int))]; 0 is full match, followed by submatches
            replacelen = length $ f m
            (skip,start,remlen) = splitAt' (off - ind) replacelen read
            remaining = replaceAt' (remlen,replacelen) txt start
        in  (off-remlen, remaining, write . (skip++)) -- start next search at beginning of last replacement rather than the end
        where
        splitAt' :: Int -> Int -> [String] -> ([String],[String],Int)
        splitAt' n replacelen xs = go n (id,xs) 
            where
            go :: Int -> (([String]->[String]),[String]) -> ([String],[String],Int)
            --go 0 (h,t) = (h [],t,0)
            --go n (h,[]) = (h [],[],n)
            go n (h,[x]) = (h [],[x],n)
            go n (h,xxs@(x:xs))
                | n > cell_len     = go (n - cell_len) (h . ([x]++),xs)
                | n == cell_len    = case replacelen of 0          -> (h [],xxs,n) -- stick daṇḍas, etc. at the end of a cell
                                                        replacelen -> (h [x],xs,0)
                | otherwise    = (h [],xxs,n)
                where cell_len = length x
        replaceAt' :: (Int,Int) -> String -> [String] -> [String]
        replaceAt' (n,m) y [x] =
            let (start,middle) = splitAt n x
                (_,end) = splitAt m middle
            in  [start ++ y ++ end]
        replaceAt' (n,m) y (x:xs) -- (start,length) replacement (list of strings)
            | m <= leftoverlen = 
                let (start,middle) = splitAt n x
                    (_,end) = splitAt m middle
                in  (start ++ y ++ end):xs
            | otherwise              =
                let (start,_) = splitAt n x
                    (replacewith,leftover) = splitAt leftoverlen y
                    new_m = m - leftoverlen
                in  (start ++ replacewith):(replaceAt' (0,new_m) leftover xs)
            where leftoverlen = (length x) - n

-- old ugly version
filterAll'' :: [Filter] -> String -> ([Filtered],String)
filterAll'' fs s = let (a,b) = go fs [] s in (reverse a,b) 
        where
        go :: [Filter] -> [Filtered] -> String -> ([Filtered],String)
        go [] us s = (us,s)
        go (x:xs) us s = (u:r1,r2)
            where 
            (r1,r2) = go xs us result
            (u,result) = replaceAll' (filterSearch x) (filterReplace x) s

filterAll' :: [Filter] -> String -> ([Filtered],String)
filterAll' fs s = go fs [] s 
        where
        go :: [Filter] -> [Filtered] -> String -> ([Filtered],String)
        go [] us s = (us,s)
        go (x:xs) us s = go xs (u:us) result
            where 
            (u,result) = replaceAll' (filterSearch x) (filterReplace x) s

unfilterAll' :: [Filtered] -> [String] -> [String]
unfilterAll' [] ss = ss
unfilterAll' (x:xs) ss = unfilterAll' xs (unreplaceAll' x ss)
