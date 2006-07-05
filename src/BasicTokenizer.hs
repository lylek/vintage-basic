-- BasicTokenizer.hs
-- Finds and tokenizes BASIC keywords, in preparation for parsing.
-- Lyle Kopnicky
-- last updated 2005-07-09

module BasicTokenizer
    (Token(..),isStringTok,isRemTok,unTok,charTokTest,tokensP) where

import Data.Char
import Parser
import SimpleParser
--import LAParser

type MyParser = SimpleParser

isEOL c = c `elem` "\n\r"

spaceTokP :: MyParser Char Token
spaceTokP = manyP' (isItP (==' ')) >> return SpaceTok

data Token = StringTok String | RemTok String | DataTok String | EOLTok
	   | CommaTok | ColonTok | SemiTok | LParenTok | RParenTok
	   | DollarTok | PercentTok
           | EqTok | NETok | LETok | LTTok | GETok | GTTok
           | PlusTok | MinusTok | MulTok | DivTok | PowTok
           | AndTok | OrTok | NotTok
           | LetTok | DimTok | OnTok | GoTok | SubTok | ReturnTok
           | IfTok | ThenTok | ForTok | ToTok | StepTok | NextTok
           | PrintTok | InputTok | RandomizeTok | ReadTok | RestoreTok
           | FnTok | EndTok
           | SpaceTok | CharTok Char
	     deriving Eq;

-- no special escape chars allowed
stringTokP = do keyP "\""
	        s <- manyP (isItP ((not . isEOL)&&-(/='\"')))
	        keyP "\""
	        return (StringTok s)

isStringTok (StringTok _) = True
isStringTok _ = False

remTokP = do keyP "REM"
             s <- manyP (isItP (not . isEOL))
             return (RemTok s)

isRemTok (RemTok _) = True
isRemTok _ = False

dataTokP = do keyP "DATA"
              s <- manyP (isItP (not . isEOL))
              return (DataTok s)
eolTokP = do manyP' (isItP isEOL); return EOLTok

charTokP = do c <- isItP (const True); return (CharTok c)

unTok (CharTok c) = c
unTok _ = error "Attempted to untokenize non-CharTok"

charTokTest f (CharTok c) = f c
charTokTest f _ = False

-- still need: numeric literals (incl labels), vars

tokenMap =
    [
     (",", CommaTok),
     (":", ColonTok),
     (";", SemiTok),
     ("(", LParenTok),
     (")", RParenTok),
     ("$", DollarTok),
     ("%", PercentTok),
     ("=", EqTok),
     ("<>", NETok),
     ("<=", LETok),
     ("<", LTTok),
     (">=", GETok),
     (">", GTTok),
     ("+", PlusTok),
     ("-", MinusTok),
     ("*", MulTok),
     ("/", DivTok),
     ("^", PowTok),
     ("AND", AndTok),
     ("OR", OrTok),
     ("NOT", NotTok),
     ("LET", LetTok),
     ("DIM", DimTok),
     ("ON", OnTok),
     ("GO", GoTok),
     ("SUB", SubTok),
     ("RETURN", ReturnTok),
     ("IF", IfTok),
     ("THEN", ThenTok),
     ("FOR", ForTok),
     ("TO", ToTok),
     ("STEP", StepTok),
     ("NEXT", NextTok),
     ("?", PrintTok),
     ("PRINT", PrintTok),
     ("INPUT", InputTok),
     ("RANDOMIZE", RandomizeTok),
     ("READ", ReadTok),
     ("RESTORE", RestoreTok),
     ("FN", FnTok),
     ("END", EndTok)
    ]

tokenP :: MyParser Char Token
tokenP = firstOfP
	 (spaceTokP ||| stringTokP ||| remTokP ||| dataTokP ||| eolTokP
	  ||| (foldl1 (|||) [do keyP s; return t | (s,t) <- tokenMap])
	  ||| charTokP)

tokensP :: MyParser Char [Token]
tokensP = manyP tokenP
