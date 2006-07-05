module Parser where

import Monad

f ||- g = \x -> f x || g x
f &&- g = \x -> f x && g x

ifEmpty [] def = def
ifEmpty xs def = xs

-- A parser contains:
-- 1) A boolean value, indicating whether the parser accepts an empty string
-- 2) A "first list" of possible input tokens the parser can accept at the
--    start of a string
-- 3) A function from a string of input tokens, returning a list of parses.
--    each parse has a result token, followed by a string containing the
--    unparsed remainder.
type Parse a b = (b,[a])
newtype Parser a b = Parser (Bool, [a], ([a] -> [Parse a b]))

(Parser (_,_,f)) $$ as = f as

instance Monad (Parser a) where
    return b = Parser $ \as -> [(b, as)]
    p >>= f = Parser $ \as -> concat [f b $$ as' | (b,as')<-(p $$ as)]

instance MonadPlus (Parser a) where
    mzero = Parser $ \as -> []
    mplus p1 p2 = Parser $ \as -> (p1 $$ as) ++ (p2 $$ as)

infixr 5 |||
(|||) :: Parser a b -> Parser a b -> Parser a b
(|||) = mplus

-- zero or one
maybeP :: Parser a b -> Parser a [b]
maybeP p = Parser $ \as ->
	    ifEmpty [([b],as') | (b,as') <- p $$ as]
		    [([],as)]

-- zero or more, longest match
manyP :: Parser a b -> Parser a [b]
manyP p = Parser $ \as ->
	    ifEmpty [(b:bs,as'') |
		     (b,as') <- p $$ as, (bs,as'') <- manyP p $$ as']
		    [([],as)]

-- one or more, longest match
manyP' :: Parser a b -> Parser a [b]
manyP' p = do b <- p
	      bs <- manyP p
	      return (b:bs)

-- first result of parser
-- due to laziness, the rest are never tried!
firstOfP :: Parser a b -> Parser a b
firstOfP p = Parser $ \as ->
	    case p $$ as
		 of [] -> []
		    (parse:_) -> [parse]

-- parses a single token according to a predicate
isitP :: (a -> Bool) -> Parser a a
isitP pred = Parser $ \as ->
	     case as of [] -> []
			(a:as') -> if pred a then [(a,as')] else []

-- for matching keywords
keyP :: Eq a => [a] -> Parser a [a]
keyP s = Parser $ \as ->
	 let (b,s') = splitAt (length s) as in
	 if b == s then [(b,s')] else []
