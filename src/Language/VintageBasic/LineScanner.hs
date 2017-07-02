-- | Reads lines of a BASIC program. It does just enough parsing to find
-- the line number.

module Language.VintageBasic.LineScanner(RawLine,rawLinesP) where

import Text.ParserCombinators.Parsec
import Language.VintageBasic.LexCommon

type RawLine = Tagged String

eol :: Parser Char
eol = do
    _ <- optionally (char '\r')
    newline

blankLineP :: Parser ()
blankLineP = do
    _ <- eol <?> ""
    whiteSpace

rawLineP :: Parser RawLine
rawLineP = do
    n <- labelP
    whiteSpace
    pos <- getPosition
    s <- manyTill (anyChar <?> "CHARACTER") (eol <?> "END OF LINE")
    whiteSpace
    _ <- many blankLineP
    return (Tagged (setSourceLine pos n) s)

rawLinesP :: Parser [RawLine]
rawLinesP = do
    whiteSpace
    _ <- many blankLineP
    ls <- many rawLineP
    _ <- many blankLineP
    eof <?> "END OF FILE"
    return ls
