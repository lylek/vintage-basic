-- SimpleParser.hs
-- A simple set of nondeterministic parser combinators.
-- Lyle Kopnicky
-- last updated 2005-07-09

module SimpleParser where

import Monad
import Parser

newtype SimpleParser a b = SimpleParser ([a] -> [(b,[a])])

instance Eq a => Monad (SimpleParser a) where
--instance (Eq a, Enum a, Bounded a) => Monad (SimpleParser a) where
    return b = SimpleParser $ \as -> [(b, as)]
    p >>= f = SimpleParser $ \as -> concat [f b $$ as' | (b,as')<-(p $$ as)]

instance Eq a => MonadPlus (SimpleParser a) where
--instance (Eq a, Enum a, Bounded a) => MonadPlus (SimpleParser a) where
    mzero = SimpleParser $ \as -> []
    mplus p1 p2 = SimpleParser $ \as -> (p1 $$ as) ++ (p2 $$ as)

instance Eq a => Parser SimpleParser a where
--instance (Eq a, Enum a, Bounded a) => Parser SimpleParser a where
    (SimpleParser f) $$ as = f as
    maybeP p = SimpleParser $ \as ->
	       ifEmpty [([b],as') | (b,as') <- p $$ as]
			   [([],as)]
    manyP p = SimpleParser $ \as ->
	      ifEmpty [(b:bs,as'') |
		       (b,as') <- p $$ as, (bs,as'') <- manyP p $$ as']
			  [([],as)]
    firstOfP p = SimpleParser $ \as ->
		 case p $$ as
		      of [] -> []
			 (parse:_) -> [parse]
    isItP pred = SimpleParser $ \as ->
		 case as of [] -> []
			    (a:as') -> if pred a then [(a,as')] else []
    keyP s = SimpleParser $ \as ->
	     let (b,s') = splitAt (length s) as in
			  if b == s then [(b,s')] else []
