-- | Contains a few common definitions for lexing BASIC.

module Language.VintageBasic.LexCommon where

import Text.ParserCombinators.Parsec

-- | A generic structure for tagging code with source positions.
data Tagged a =
    Tagged { getPosTag :: SourcePos, getTaggedVal :: a }
    deriving (Show)

instance (Eq a) => Eq (Tagged a) where
    (Tagged _ x) == (Tagged _ y) = x == y

-- | Parses a single whitespace character.
whiteSpaceChar :: Parser Char
whiteSpaceChar = oneOf " \v\f\t" <?> "SPACE"

-- | Parses a stretch of whitespace.
whiteSpace :: Parser ()
whiteSpace = skipMany (whiteSpaceChar <?> "")

-- | Parses a legal BASIC character.
legalChar :: Parser Char
legalChar = letter <|> digit <|> oneOf ",:;()$%=<>+-*/^?"

-- | Parses a line number.
labelP :: Parser Int
labelP = do
    s <- many1 (digit <?> "") <?> "LINE NUMBER"
    return (read s)

-- | Parser that skips text specified by another parser, if present.
skip0or1 :: GenParser tok st a -> GenParser tok st ()
skip0or1 parser = (parser >> return ()) <|> return ()
