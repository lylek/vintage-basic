{-# OPTIONS_GHC -fglasgow-exts #-}

-- Parser.hs
-- Type class defining the combinators required of a nondeterministic parser.
-- Lyle Kopnicky

module Parser where

import Monad

f ||- g = \x -> f x || g x
f &&- g = \x -> f x && g x

ifEmpty [] def = def
ifEmpty xs def = xs

type Parse a b = (b,[a])

class (Monad (t a), MonadPlus (t a), Eq a) => Parser t a
--class (Monad (t a), MonadPlus (t a), Eq a, Enum a, Bounded a) => Parser t a
    where
    ($$) :: t a b -> [a] -> [Parse a b]
    -- zero or one
    maybeP :: t a b -> t a [b]
    -- zero or more, longest match
    manyP :: t a b -> t a [b]
    -- first result of parser
    -- due to laziness, the rest are never tried!
    firstOfP :: t a b -> t a b
    -- parses a single token according to a predicate
    isItP :: (a -> Bool) -> t a a
    -- for matching keywords
    keyP :: [a] -> t a [a]

infixr 5 |||
(|||) :: Parser a b => a b c -> a b c -> a b c
(|||) = mplus

-- one or more, longest match
manyP' :: Parser t a => t a b -> t a [b]
manyP' p = do b <- p
	      bs <- manyP p
	      return (b:bs)
