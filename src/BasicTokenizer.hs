-- BasicTokenizer.hs
-- Finds and tokenizes BASIC keywords, in preparation for parsing.
-- Lyle Kopnicky

module BasicTokenizer
    (PrimToken(..),Token,isDataTok,isRemTok,unTok,unCharTok,charTokTest,
     tokenize,tokenP,printToken) where

import Text.ParserCombinators.Parsec
import BasicLexCommon

spaceTokP :: Parser PrimToken
spaceTokP = whiteSpaceChar >> whiteSpace >> return SpaceTok

data PrimToken = StringTok String | RemTok String | DataTok String
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
             deriving (Eq,Show)

keyword :: String -> Parser String
keyword s = try (string s) <?> ("keyword " ++ s)

stringTokP :: Parser PrimToken
-- no special escape chars allowed
stringTokP =
    do char '"'
       s <- manyTill anyChar (char '"')
       whiteSpace
       return (StringTok s)

isStringTok :: PrimToken -> Bool
isStringTok (StringTok _) = True
isStringTok _ = False

remTokP :: Parser PrimToken
remTokP = do keyword "REM"
             s <- many anyChar
             return (RemTok s)

isRemTok :: PrimToken -> Bool
isRemTok (RemTok _) = True
isRemTok _ = False

dataTokP :: Parser PrimToken
dataTokP =
    do keyword "DATA"
       s <- many anyChar
       return (DataTok s)

isDataTok :: PrimToken -> Bool
isDataTok (DataTok _) = True
isDataTok _ = False

charTokP :: Parser PrimToken
charTokP = do c <- legalChar; return (CharTok c)

isCharTok :: PrimToken -> Bool
isCharTok (CharTok _) = True
isCharTok _ = False

unCharTok :: Token -> Char
unCharTok (_, (CharTok c)) = c
unCharTok (_, _) = error "Attempted to untokenize non-CharTok"

charTokTest :: (Char -> Bool) -> PrimToken -> Bool
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
     ("PRINT", PrintTok),
     ("?", PrintTok),
     ("INPUT", InputTok),
     ("RANDOMIZE", RandomizeTok),
     ("READ", ReadTok),
     ("RESTORE", RestoreTok),
     ("FN", FnTok),
     ("END", EndTok)
    ]

revTokenMap = [(t,s) | (s,t) <- tokenMap]

anyTokP :: Parser PrimToken
anyTokP = choice ([spaceTokP, stringTokP, remTokP, dataTokP]
                  ++ [do keyword s; whiteSpace; return t | (s,t) <- tokenMap]
                  ++ [charTokP]) <?> "legal BASIC character"

type Token = (SourcePos,PrimToken)

unTok :: Token -> PrimToken
unTok (pos,tok) = tok

tokTest :: (PrimToken -> Bool) -> Token -> Bool
tokTest test (pos,tok) = test tok

tokenize1 :: Parser Token
tokenize1 =
    do pos <- getPosition
       tok <- anyTokP
       return (pos,tok)

tokenize :: Parser [Token]
tokenize =
    do tokens <- many tokenize1
       eof <?> ""
       return tokens

-- The single-token parser used at the parser level
tokenP :: (PrimToken -> Maybe PrimToken) -> GenParser Token () Token
tokenP test = token printToken posToken testToken
      where testToken (pos,tok) =
		case test tok
		     of Nothing -> Nothing
			(Just t) -> Just (pos, t)

printToken (pos,tok) =
    case (lookup tok revTokenMap)
         of (Just s) -> s
	    Nothing ->
		case tok
		     of (CharTok c) -> [c]
			(DataTok s) -> "DATA" ++ s
			(RemTok s) -> "REM" ++ s
			SpaceTok -> " "
			(StringTok s) -> "\"" ++ s ++ "\""
			otherwise -> error "showToken: unrecognized token."

posToken :: Token -> SourcePos
posToken (pos,tok) = pos
