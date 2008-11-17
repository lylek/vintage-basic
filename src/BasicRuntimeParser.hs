{-# OPTIONS_GHC #-}

-- BasicRuntimeParser.hs
-- Parsing for DATA statements and input buffer
-- Lyle Kopnicky

module BasicRuntimeParser(dataValsP,readFloat) where

import Text.ParserCombinators.Parsec
import BasicFloatParser

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
       return s

trim :: String -> String
trim s = dropWhile (==' ') $ reverse $ dropWhile (==' ') $ reverse s

dataValP :: Parser String
dataValP =
    do s <- stringP <|> many nonCommaP
       return (trim s)

dataValsP :: Parser [String]
dataValsP = sepBy1 dataValP (char ',')
