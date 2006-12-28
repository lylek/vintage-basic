-- BasicLexCommon.hs
-- Contains a few common definitions for lexing BASIC
-- Lyle Kopnicky

module BasicLexCommon where

import Text.ParserCombinators.Parsec

whiteSpaceChar :: Parser Char
whiteSpaceChar = oneOf " \v\f\t" <?> "space"

whiteSpace :: Parser ()
whiteSpace = skipMany (whiteSpaceChar <?> "")

legalChar :: Parser Char
legalChar = letter <|> digit <|> oneOf ",:;()$%=<>+-*/^?"

labelP :: Parser Int
labelP =
    do s <- many1 (digit <?> "") <?> "line number"
       return (read s)
