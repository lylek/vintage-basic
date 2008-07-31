-- BasicLineScanner.hs
-- Reads lines of a BASIC program, including line numbers
-- Lyle Kopnicky

module BasicLineScanner(RawLine,rawLinesP) where

import Text.ParserCombinators.Parsec
import BasicLexCommon

type RawLine = Tagged String

blankLineP :: Parser ()
blankLineP = do
    newline <?> ""
    whiteSpace

rawLineP :: Parser RawLine
rawLineP = do
    n <- labelP
    whiteSpace
    pos <- getPosition
    s <- manyTill (anyChar <?> "character") newline
    whiteSpace
    many blankLineP
    return (Tagged (setSourceLine pos n) s)

rawLinesP :: Parser [RawLine]
rawLinesP = do
    whiteSpace
    many blankLineP
    ls <- many rawLineP
    many blankLineP
    eof <?> "end of file"
    return ls
