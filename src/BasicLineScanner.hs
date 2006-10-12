-- BasicLineScanner.hs
-- Reads lines of a BASIC program, including line numbers
-- Lyle Kopnicky

module BasicLineScanner(RawLine(..),rawLinesP) where

import Text.ParserCombinators.Parsec
import BasicLexCommon

data RawLine = RawLine Line Column String
	       deriving (Show,Eq)

rawLineP :: Parser RawLine
rawLineP =
    do whiteSpace
       n <- labelP
       whiteSpace
       pos <- getPosition
       s <- manyTill (anyChar <?> "character") newline
       return (RawLine n (sourceColumn pos) s)

rawLinesP :: Parser [RawLine]
rawLinesP =
    do ls <- many rawLineP
       eof <?> "end of file"
       return ls
