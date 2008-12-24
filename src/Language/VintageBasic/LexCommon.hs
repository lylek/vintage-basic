-- BasicLexCommon.hs
-- Contains a few common definitions for lexing BASIC
-- Lyle Kopnicky

module Language.VintageBasic.LexCommon where

import Text.ParserCombinators.Parsec

data Tagged a =
    Tagged { getPosTag :: SourcePos, getTaggedVal :: a }
    deriving (Show)

instance (Eq a) => Eq (Tagged a) where
    (Tagged _ x) == (Tagged _ y) = x == y

whiteSpaceChar :: Parser Char
whiteSpaceChar = oneOf " \v\f\t" <?> "SPACE"

whiteSpace :: Parser ()
whiteSpace = skipMany (whiteSpaceChar <?> "")

legalChar :: Parser Char
legalChar = letter <|> digit <|> oneOf ",:;()$%=<>+-*/^?"

labelP :: Parser Int
labelP = do
    s <- many1 (digit <?> "") <?> "LINE NUMBER"
    return (read s)

skip0or1 :: GenParser tok st a -> GenParser tok st ()
skip0or1 parser = (parser >> return ()) <|> return ()
