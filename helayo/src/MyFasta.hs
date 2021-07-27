module MyFasta (
fasta',
fastaFile',
parseFasta'
) where

import Data.Char
import Text.Parsec
import Control.Monad (void)
import qualified Data.Map as Map
import qualified System.IO as IO
import qualified Data.String.Utils as S
import Data.Fasta.String.Types

eol :: Parsec String u String
eol = choice . map (try . string) $ ["\n\r", "\r\n", "\n", "\r"]

eoe :: Parsec String u ()
eoe  = do
    lookAhead (void $ char '>') <|> eof

fasta' :: Parsec String u FastaSequence
fasta' = do
    spaces
    char '>'
    header <- manyTill (satisfy (/= '>')) eol
    fseq <- manyTill anyChar eoe
    return ( FastaSequence { fastaHeader = S.strip header
                           , fastaSeq    = removeNewline fseq } )
  where
    removeNewline = filter (`notElem` "\n\r")

fastaFile' :: Parsec String u [FastaSequence]
fastaFile'= do
    spaces
    many fasta'

parseFasta' :: String -> [FastaSequence]
parseFasta' = eToV . parse fastaFile' "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)
