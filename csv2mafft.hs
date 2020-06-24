import System.Environment
import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Transcribe

main = do
    (fn:args) <- getArgs
    contents <- readFile fn
    let fl = lines contents
    let result = mafftMatrix $ makeMatrix $ makeArray fl
    mapM_ (mapM_ putStrLn) result

makeArray :: [String] -> [[String]]
makeArray sss = map strToArr sss
    where strToArr = wordsBy (== ';')

data Matrix = Matrix {columns :: [String],
                      rows :: [String],
                      matrix :: [[String]]} deriving (Show)

makeMatrix :: [[String]] -> Matrix
makeMatrix (ss:sss) = Matrix {columns = xaxis, rows = yaxis, matrix = scores}
    where
        xaxis = map (\s -> concat . map charToHex $ transliterateString s iast slp1) ss
        yaxis = map (\ss -> concat . map charToHex $ transliterateString (head ss) iast slp1) sss
        size = length xaxis * length yaxis - 1
        scores = map (drop 1) sss

mafftMatrix :: Matrix -> [[String]]
mafftMatrix m = map (\s -> map (\t -> s ++ (' ':t) ++ (' ':(score s t))) xaxis) yaxis
    where
        xaxis = rows m
        yaxis = columns m
        score :: String -> String -> String
        score x y
            | xindex == Nothing = "X"
            | yindex == Nothing = "X"
            | otherwise         = (matrix m !! (cleanup xindex)) !! (cleanup yindex)
            where
                xindex = x `elemIndex` xaxis
                yindex = y `elemIndex` yaxis
                cleanup (Just a) = a
                cleanup Nothing = 0

charToHex :: Char -> String
charToHex c = "0x" ++ (intToHex . ord) c

intToHex :: Int -> String
intToHex = map toUpper . reverse . loop
    where 
        loop i
            | i < 16 = [intToDigit i]
            | otherwise     = (intToDigit r) : loop q
            where (q,r) = quotRem i 16
