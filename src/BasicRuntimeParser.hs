-- BasicRuntimeParser.hs
-- Parsing for DATA statements and input buffer
-- Lyle Kopnicky

module BasicRuntimeParser(dataValsP,readFloat) where

import Text.ParserCombinators.Parsec

readFloat :: String -> Maybe Float
readFloat s =
    case parse floatP "" s
         of (Right fv) -> Just fv
            _ -> Nothing

floatP :: Parser Float
floatP =
    do sgn <- option "" sgnP
       mant <- try float2P <|> float1P
       exp <- option "" expP
       return (read (sgn++mant++exp))

float1P :: Parser String
float1P = many1 digit

float2P :: Parser String
float2P =
    do i <- many digit
       char '.'
       f <- many digit
       return ("0"++i++"."++f++"0")

sgnP :: Parser String
sgnP =
    do sgn <- char '+' <|> char '-'
       return (if sgn == '+' then "" else "-")

expP :: Parser String
expP =
    do char 'E'
       esgn <- option "" sgnP
       i <- many1 digit
       return ("E"++esgn++i)

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
