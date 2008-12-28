{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Code for parsing floats. Used both in parsing static code and at runtime
-- (@INPUT@ and @DATA@ statements).

module Language.VintageBasic.FloatParser(FloatParser(..)) where

import Data.Char(isDigit)
import Text.ParserCombinators.Parsec
import Language.VintageBasic.LexCommon(Tagged,getTaggedVal)
import Language.VintageBasic.Tokenizer(Token(..),tokenP,charTokTest)

-- | Generic class for float parsers. Can be used to parse floats from raw
-- characters (useful at runtime) or from tokenized text (for parsing source).
class FloatParser tok st where
    digitP :: GenParser tok st Char
    dotP   :: GenParser tok st Char
    plusP  :: GenParser tok st Char
    minusP :: GenParser tok st Char
    charEP :: GenParser tok st Char

    floatP :: GenParser tok st Float
    floatP = do
        sgn <- option "" sgnP
        mant <- try float2P <|> float1P
        expt <- option "" expP
        return (read (sgn++mant++expt))

    float1P :: GenParser tok st String
    float1P = many1 digitP

    float2P :: GenParser tok st String
    float2P = do
        i <- many digitP
        dotP
        f <- many digitP
        return ("0"++i++"."++f++"0")

    sgnP :: GenParser tok st String
    sgnP = do
        sgn <- plusP <|> minusP
        return (if sgn == '+' then "" else "-")

    expP :: GenParser tok st String
    expP = do
        charEP
        esgn <- option "" sgnP
        i <- many1 digitP
        return ("E"++esgn++i)

instance FloatParser Char st where
    digitP = digit
    dotP   = char '.'
    plusP  = char '+'
    minusP = char '-'
    charEP = char 'E' <|> char 'e'

instance FloatParser (Tagged Token) () where
    digitP = do tok <- tokenP (charTokTest isDigit)
                return (getCharTokChar $ getTaggedVal tok)
    dotP   = tokenP (==DotTok)            >> return '.'
    plusP  = tokenP (==PlusTok)           >> return '+'
    minusP = tokenP (==MinusTok)          >> return '-'
    charEP = tokenP (charTokTest (=='E')) >> return 'E'
