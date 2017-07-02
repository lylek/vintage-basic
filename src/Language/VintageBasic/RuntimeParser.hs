-- | Parsing for @DATA@ statements and the @INPUT@ buffer.

module Language.VintageBasic.RuntimeParser(dataValsP,readFloat,trim) where

import Language.VintageBasic.FloatParser(floatP)
import Text.ParserCombinators.Parsec

-- | Attempts to parse a floating point value from a string.
readFloat :: String -> Maybe Float
readFloat s =
    case parse floatP "" s
         of (Right fv) -> Just fv
            _ -> Nothing

nonCommaP :: Parser Char
nonCommaP = satisfy (/=',')

stringP :: Parser String
stringP =
    do _ <- char '"'
       s <- manyTill anyChar (char '"')
       spaces
       return s

-- | Trim leading and trailing space from a string.
trim :: String -> String
trim s = dropWhile (==' ') $ reverse $ dropWhile (==' ') $ reverse s

-- | Parse a single data value.
dataValP :: Parser String
dataValP = do
    spaces
    stringP <|> do { s <- many nonCommaP; return (trim s) }

-- | Parse a list of data values.
-- Works for both @INPUT@ text and @DATA@ statements.
dataValsP :: Parser [String]
dataValsP = sepBy1 dataValP (char ',')
