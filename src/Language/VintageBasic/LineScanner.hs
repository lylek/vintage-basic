-- | Reads lines of a BASIC program. It does just enough parsing to find
-- the line number.

module Language.VintageBasic.LineScanner(RawLine,rawLinesP) where

import Text.ParserCombinators.Parsec
import Language.VintageBasic.LexCommon

type RawLine = Tagged String

eol :: Parser Char
eol = do
    optionally (char '\r')
    newline

blankLineP :: Parser ()
blankLineP = do
    eol <?> ""
    whiteSpace

rawLineP :: Parser RawLine
rawLineP = do
    n <- labelP
    whiteSpace
    pos <- getPosition
    s <- manyTill (anyChar <?> "CHARACTER") (eol <?> "NEW-LINE")
    whiteSpace
    many blankLineP
    return (Tagged (setSourceLine pos n) s)

rawLinesP :: Parser [RawLine]
rawLinesP = do
    whiteSpace
    many blankLineP
    ls <- many rawLineP
    many blankLineP
    eof <?> "END OF FILE"
    return ls
