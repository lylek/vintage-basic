-- BasicParser.hs
-- Parses BASIC source code to produce abstract syntax.
-- Also used at runtime to input values.
-- Lyle Kopnicky
-- last updated 2005-07-09

module BasicParser(linesP,readFloat,dataValsP) where

import Data.Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import BasicSyntax
import BasicTokenizer
import BasicLexCommon

-- TODO: worry about case sensitivity
-- TODO: think about when to use 'try'

-- only first 2 chars and 1st digit of variable names are siginificant
-- should make this settable by option
varSignifLetters = 2 :: Int
varSignifDigits = 1 :: Int

type TokParser = GenParser Token ()

skipSpace :: TokParser ()
skipSpace = skipMany (tokenP (==SpaceTok))

-- LITERALS

floatLitP :: TokParser Literal
floatLitP =
    do v <- floatP
       return (FloatLit v)

sgnP :: TokParser String
sgnP =
    do sgn <- tokenP (==PlusTok) <|> tokenP (==MinusTok)
       return (if tokTest (==PlusTok) sgn then "" else "-")

floatP :: TokParser Float
floatP =
    do skipSpace
       sgn <- option "" sgnP
       mant <- try float2P <|> float1P
       exp <- option "" expP
       return (read (sgn++mant++exp))

expP :: TokParser String
expP =
    do tokenP (charTokTest (=='E'))
       esgn <- option "" sgnP
       i <- many1 (charTokTest isDigit)
       return ("E"++esgn++i)

float1P :: TokParser String
float1P = many1 (charTokTest isDigit)

float2P :: TokParser String
float2P =
    do i <- many (charTokTest isDigit)
       tokenP (charTokTest (=='.'))
       f <- many (charTokTest isDigit)
       return ("0"++i++"."++f++"0")

stringLitP :: TokParser Literal
stringLitP =
    do (StringTok cs) <- tokenP isStringTok
       return (StringLit cs)

litP :: TokParser Literal
litP = floatLitP <|> stringLitP

-- VARIABLES

varBaseP :: Parser String
varBaseP = do ls <- many1 (do notFollowedBy keywordP; upper)
              ds <- many digit
              return (take varSignifLetters ls
                      ++ take varSignifDigits ds)

keywordP :: Parser String
keywordP =
    choice $ map (try . string)
	       ["AND", "OR", "NOT", "LET", "DIM", "ON", "GO", "SUB", "RETURN",
		"IF", "THEN", "FOR", "TO", "STEP", "NEXT", "PRINT", "INPUT",
		"RANDOMIZE", "READ", "RESTORE", "FN", "END"]

floatVarP :: Parser Var
floatVarP = do name <- varBaseP
               return (FloatVar name [])

intVarP :: Parser Var
intVarP = do name <- varBaseP
             char '%'
             return (IntVar name [])

stringVarP :: Parser Var
stringVarP = do name <- varBaseP
                char '$'
                return (StringVar name [])

-- Look for string and int vars first because of $ and % suffixes.
simpleVarP :: Parser Var
simpleVarP = do spaces; try stringVarP <|> try intVarP <|> floatVarP

arrP :: Parser Var
arrP =
    do v <- simpleVarP
       xs <- argsP
       return (case v
                 of FloatVar name [] -> FloatVar name xs
                    IntVar name [] -> IntVar name xs
                    StringVar name [] -> StringVar name xs)

varP :: Parser Var
varP = try arrP <|> simpleVarP

-- EXPRESSIONS

litXP :: Parser Expr
litXP =
    do v <- litP
       return (LitX v)

varXP :: Parser Expr
varXP =
    do v <- varP
       return (VarX v)

argsP :: Parser Expr
argsP =
    do char '('
       spaces
       xs <- sepBy exprP (spaces >> char ',' >> spaces)
       spaces
       char ')'
       return xs

parenXP :: Parser Expr
parenXP =
    do char '('
       spaces
       x <- exprP
       spaces
       char ')'
       return (ParenX x)

primXP :: Parser Expr
primXP = parenXP <|> litXP <|> varXP

opTable :: OperatorTable Char () Expr
opTable =
    [[prefix "-" MinusX, prefix "+" id],
     [binary "^" (BinX PowOp) AssocRight],
     [binary "*" (BinX MulOp) AssocLeft, binary "/" (BinX DivOp) AssocLeft],
     [binary "+" (BinX AddOp) AssocLeft, binary "-" (BinX SubOp) AssocLeft]
     [binary "=" (BinX EqOp) AssocLeft, binary "<>" (BinX NEOp) AssocLeft,
      binary "<" (BinX LTOp) AssocLeft, binary "<=" (BinX LEOp) AssocLeft,
      binary ">" (BinX GTOp) AssocLeft, binary ">=" (BinX GEOp) AssocLeft],
     [prefix "NOT" NotX],
     [binary "AND" (BinX AndOp) AssocLeft],
     [binary "OR" (BinX OrOp) AssocLeft]]

binary :: String -> a -> Assoc -> Operator Char () Expr
binary name fun assoc =
    Infix (do spaces; string name; spaces; return fun) assoc
prefix :: String -> a -> Assoc -> Operator Char () Expr
prefix name fun =
    Prefix (do spaces; string name; spaces; return fun)

exprP :: Parser Expr
exprP = buildExpressionParser opTable primXP

-- STATEMENTS

letSP :: Parser Statement
letSP =
    do string "LET"
       spaces
       v <- varP
       spaces
       char '='
       spaces
       x <- exprP
       return (LetS v x)

gotoSP :: Parser Statement
gotoSP =
    do string "GO"
       spaces
       string "TO"
       spaces
       n <- labelP
       return (GotoS n)

gosubSP :: Parser Statement
gosubSP =
    do string "GO"
       spaces
       string "SUB"
       spaces
       n <- labelP
       return (GosubS n)

returnSP :: Parser Statement
returnSP =
    do string "RETURN"
       return ReturnS

ifSP :: Parser Statement
ifSP =
    do string "IF"
       spaces
       x <- exprP
       spaces
       string "THEN"
       spaces
       target <- try ifSPGoto <|> statementListP
       return (IfS x target)

ifSPGoto :: Parser [Statement]
ifSPGoto =
    do n <- labelP
       return [GotoS n]

forSP :: Parser Statement
forSP =
    do string "FOR"
       spaces
       v <- simpleVarP
       spaces
       char '='
       spaces
       x1 <- exprP
       spaces
       string "TO"
       spaces
       x2 <- exprP
       spaces
       x3 <- option (LitX (FloatLit 1)) (string "STEP" >> spaces >> exprP)
       return (ForS v x1 x2 x3)

-- handles a NEXT and an optional variable list
nextSP :: Parser Statement
nextSP =
    do string "NEXT"
       spaces
       vs <- sepBy simpleVarP (spaces >> char ',' >> spaces)
       if length vs > 0
          then return (NextS (Just vs))
	  else return (NextS Nothing)

printSP :: Parser Statement
printSP =
    do string "PRINT"
       spaces
       xs <- sepBy exprP (spaces >> char ';' >> spaces)
       (char ';' >> return (PrintS xs False))
           <|> return (PrintS xs True)

inputSP :: Parser Statement
inputSP =
    do string "INPUT"
       spaces
       ps <- option (return Nothing) inputPrompt
       spaces
       vs <- sepBy1 varP (spaces >> char ',' >> spaces)
       return (InputS ps vs)

inputPrompt :: Parser Statement
inputPrompt =
    do (StringLit p) <- stringLitP
       spaces
       char ';'
       return Just p

endSP :: Parser Statement
endSP =
    do string "END"
       return EndS

dimSP :: Parser Statement
dimSP =
    do string "DIM"
       spaces
       arr <- arrP
       return (DimS arr)

remSP :: Parser Statement
remSP =
    do string "REM"
       s <- many anyChar
       return (RemS s)

statementP :: Parser Statement
statementP =
    choice $ map try [printSP, inputSP, gotoSP, gosubSP, returnSP,
                      ifSP, forSP, nextSP, endSP, dimSP, remSP, letSP]

statementListP :: Parser [Statement]
statementListP =
    do sepEndBy1 statementP (spaces >> many1 (char ':' >> spaces))

-- LINES

lineP :: Parser (Int,String)
lineP =
    do spaces
       n <- labelP
       s <- manyTill anyChar newline
       return (n,s)

linesP :: Parser [(Int,String)]
linesP = many lineP

-- DATA STATEMENTS / INPUT BUFFER

-- We don't need to look for EOL characters, because these will only be
-- fed single lines.

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
