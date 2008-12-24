-- BasicRuntimeParser.hs
-- Parsing for DATA statements and input buffer
-- Lyle Kopnicky

module Language.VintageBasic.RuntimeParser(dataValsP,readFloat,trim) where

import Text.ParserCombinators.Parsec
import Language.VintageBasic.FloatParser

readFloat :: String -> Maybe Float
readFloat s =
    case parse floatP "" s
         of (Right fv) -> Just fv
            _ -> Nothing

nonCommaP :: Parser Char
nonCommaP = satisfy (/=',')

stringP :: Parser String
stringP =
    do char '"'
       s <- manyTill anyChar (char '"')
       spaces
       return s

trim :: String -> String
trim s = dropWhile (==' ') $ reverse $ dropWhile (==' ') $ reverse s

dataValP :: Parser String
dataValP = do
    spaces
    stringP <|> do { s <- many nonCommaP; return (trim s) }

dataValsP :: Parser [String]
dataValsP = sepBy1 dataValP (char ',')
