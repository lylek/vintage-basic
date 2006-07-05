-- BasicTokenizer.hs
-- Finds and tokenizes BASIC keywords, in preparation for parsing.
-- Lyle Kopnicky
-- last updated 2005-07-05

module BasicTokenizer where

import Data.Char
import Parser

isEOL c = c `elem` "\n\r"

spaceP :: Parser t Char => t Char String
spaceP = manyP (isitP (==' '))
keywP :: Parser t Char => String -> t Char String
keywP s = do spaceP; keyP s

data Token = StringTok String
           | AndTok | OrTok | NotTok
           | EqTok | NETok | LTEqTok | LTTok | GTEqTok | GTTok
           | AddTok | SubTok | MulTok | DivTok
           | LetTok | DimTok | GotoTok | GosubTok | ReturnTok
           | IfTok | ThenTok | ForTok | ToTok | StepTok | NextTok
           | PrintTok | InputTok | EndTok | RemTok String
           | EOLTok | CharTok Char

-- no special escape chars allowed
stringTokP :: Parser t Char => t Char Token
stringTokP = do keyP "\""
	        s <- manyP (isitP ((not . isEOL)&&-(/='\"')))
	        keyP "\""
	        return (StringTok s)

andTokP :: Parser t Char => t Char Token
andTokP = do keyP "AND"; return AndTok
orTokP :: Parser t Char => t Char Token
orTokP = do keyP "OR"; return OrTok
notTokP :: Parser t Char => t Char Token
notTokP = do keyP "NOT"; return NotTok
letTokP :: Parser t Char => t Char Token
letTokP = do keyP "LET"; return LetTok
dimTokP :: Parser t Char => t Char Token
dimTokP = do keyP "DIM"; return DimTok
gotoTokP :: Parser t Char => t Char Token
gotoTokP = do keyP "GOTO"; return GotoTok
gosubTokP :: Parser t Char => t Char Token
gosubTokP = do keyP "GOSUB"; return GosubTok
returnTokP :: Parser t Char => t Char Token
returnTokP = do keyP "RETURN"; return ReturnTok
ifTokP :: Parser t Char => t Char Token
ifTokP = do keyP "IF"; return IfTok
thenTokP :: Parser t Char => t Char Token
thenTokP = do keyP "THEN"; return ThenTok
forTokP :: Parser t Char => t Char Token
forTokP = do keyP "FOR"; return ForTok
toTokP :: Parser t Char => t Char Token
toTokP = do keyP "TO"; return ToTok
stepTokP :: Parser t Char => t Char Token
stepTokP = do keyP "STEP"; return StepTok
nextTokP :: Parser t Char => t Char Token
nextTokP = do keyP "NEXT"; return NextTok
printTokP :: Parser t Char => t Char Token
printTokP = do firstOfP (keyP "?" ||| keyP "PRINT"); return PrintTok
inputTokP :: Parser t Char => t Char Token
inputTokP = do keyP "INPUT"; return InputTok
endTokP :: Parser t Char => t Char Token
endTokP = do keyP "END"; return EndTok
remTokP :: Parser t Char => t Char Token
remTokP = do keyP "REM"
             s <- manyP (isitP (not . isEOL))
             return (RemTok s)
eolTokP :: Parser t Char => t Char Token
eolTokP = do manyP' (isitP isEOL); return EOLTok
charTokP :: Parser t Char => t Char Token
charTokP = do c <- isitP (const True); return (CharTok c)
