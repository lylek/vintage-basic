-- LAParser.hs
-- A Parser transformer which provides single-character lookahead, for
-- efficiency.
-- Lyle Kopnicky
-- last updated 2005-07-09

module LAParser where

import Monad
import Parser
import Data.List

-- A lookahead parser contains:
-- 1) A list of what the parser returns when you feed it an empty string
-- 2) A "first list" of possible input tokens the parser can accept at the
--    start of a string
-- 3) A parser from input tokens to output tokens.
data Parser p a => LAParser p a b = LAParser [b] [a] (p a b)

instance Parser p a => Monad (LAParser p a) where
    return (b :: b) = LAParser [b] [] (return b :: p a b)
    (LAParser e1 l1 p1) >>= f =
	(let els = [let (LAParser e2 l2 p2) = f e in (e2,l2) | e <- e1] in
	 (LAParser
	  (concatMap fst els)
	  (l1 `union` (foldl1 union (map snd els)))
	  (p1 >>= (\b -> let (LAParser e2 l2 p2) = f b in p2))))

instance Parser p a => MonadPlus (LAParser p a) where
    mzero = LAParser [] [] mzero
    mplus (LAParser e1 l1 p1) (LAParser e2 l2 p2) =
	LAParser (e1 ++ e2) (l1 `union` l2) (p1 `mplus` p2)

instance Parser p a => Parser (LAParser p) a where
    (LAParser _ _ p) $$ as = p $$ as
    maybeP (LAParser e l p) = LAParser ([] : [[b] | b <- e]) l (maybeP p)
    manyP (LAParser e l p) = LAParser ([] : [[b] | b <- e]) l (manyP p)
    firstOfP (LAParser e l p) = LAParser e l (firstOfP p)
    isItP pred = LAParser [] (filter pred [minBound..maxBound]) (isItP pred)
    keyP [] = LAParser [[]] [] (keyP [])
    keyP s = LAParser [] [(head s)] (keyP s)
