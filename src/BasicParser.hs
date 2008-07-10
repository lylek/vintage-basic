{-# OPTIONS_GHC -fglasgow-exts #-}

-- BasicParser.hs
-- Parses BASIC source code to produce abstract syntax.
-- Also used at runtime to input values.
-- Lyle Kopnicky

module BasicParser(statementListP) where

import Data.Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import BasicFloatParser
import BasicLexCommon
import BasicSyntax
import BasicTokenizer

-- TODO: think about when to use 'try'

-- only first 2 chars and 1st digit of variable names are siginificant
-- should make this settable by option
varSignifLetters = 2 :: Int
varSignifDigits = 1 :: Int

type TokParser = GenParser (Tagged Token) ()

skipSpace :: TokParser ()
skipSpace = skipMany $ tokenP (==SpaceTok)

lineNumP :: TokParser Int
lineNumP =
    do s <- many1 (tokenP (charTokTest isDigit) <?> "") <?> "line number"
       return (read (map (getCharTokChar . getTaggedVal) s))

-- LITERALS

floatLitP :: TokParser Literal
floatLitP =
    do v <- floatP
       skipSpace
       return (FloatLit v)

instance BasicFloatParser (Tagged Token) () where
    digitP = do tok <- tokenP (charTokTest isDigit)
                return (getCharTokChar $ getTaggedVal tok)
    dotP   = tokenP (==DotTok)            >> return '.'
    plusP  = tokenP (==PlusTok)           >> return '+'
    minusP = tokenP (==MinusTok)          >> return '-'
    charEP = tokenP (charTokTest (=='E')) >> return 'E'

stringLitP :: TokParser Literal
stringLitP =
    do tok <- tokenP isStringTok
       return (StringLit (getStringTokString (getTaggedVal tok)))

litP :: TokParser Literal
litP = floatLitP <|> stringLitP

-- VARIABLES

varBaseP :: TokParser String
varBaseP = do ls <- many1 (tokenP (charTokTest isAlpha))
              ds <- many (tokenP (charTokTest isDigit))
              return (taggedCharToksToString (take varSignifLetters ls
                      ++ take varSignifDigits ds))

floatVarP :: TokParser Var
floatVarP = do name <- varBaseP
               return (FloatVar name [])

intVarP :: TokParser Var
intVarP = do name <- varBaseP
             tokenP (==PercentTok)
             return (IntVar name [])

stringVarP :: TokParser Var
stringVarP = do name <- varBaseP
                tokenP (==DollarTok)
                return (StringVar name [])

-- Look for string and int vars first because of $ and % suffixes.
simpleVarP :: TokParser Var
simpleVarP = do
    v <- try stringVarP <|> try intVarP <|> floatVarP
    skipSpace
    return v

arrP :: TokParser Var
arrP =
    do v <- simpleVarP
       xs <- argsP
       skipSpace
       return (case v
                 of FloatVar name [] -> FloatVar name xs
                    IntVar name [] -> IntVar name xs
                    StringVar name [] -> StringVar name xs)

varP :: TokParser Var
varP = try arrP <|> simpleVarP

-- BUILTINS

builtinXP :: TokParser Expr
builtinXP = do
    (Tagged _ (BuiltinTok b)) <- tokenP isBuiltinTok
    xs <- argsP
    return (BuiltinX b xs)

-- EXPRESSIONS

litXP :: TokParser Expr
litXP =
    do v <- litP
       return (LitX v)

varXP :: TokParser Expr
varXP =
    do v <- varP
       return (VarX v)

argsP :: TokParser [Expr]
argsP =
    do tokenP (==LParenTok)
       xs <- sepBy exprP (tokenP (==CommaTok))
       tokenP (==RParenTok)
       return xs

parenXP :: TokParser Expr
parenXP =
    do tokenP (==LParenTok)
       x <- exprP
       tokenP (==RParenTok)
       return (ParenX x)

primXP :: TokParser Expr
primXP = parenXP <|> litXP <|> builtinXP <|> varXP

opTable :: OperatorTable (Tagged Token) () Expr
opTable =
    [[prefix MinusTok MinusX, prefix PlusTok id],
     [binary PowTok  (BinX PowOp) AssocRight],
     [binary MulTok  (BinX MulOp) AssocLeft, binary DivTok   (BinX DivOp) AssocLeft],
     [binary PlusTok (BinX AddOp) AssocLeft, binary MinusTok (BinX SubOp) AssocLeft],
     [binary EqTok   (BinX EqOp)  AssocLeft, binary NETok    (BinX NEOp)  AssocLeft,
      binary LTTok   (BinX LTOp)  AssocLeft, binary LETok    (BinX LEOp)  AssocLeft,
      binary GTTok   (BinX GTOp)  AssocLeft, binary GETok    (BinX GEOp)  AssocLeft],
     [prefix NotTok   NotX],
     [binary AndTok  (BinX AndOp) AssocLeft],
     [binary OrTok   (BinX OrOp)  AssocLeft]]

binary :: Token -> (Expr -> Expr -> Expr) -> Assoc -> Operator (Tagged Token) () Expr
binary tok fun assoc =
    Infix (do tokenP (==tok); return fun) assoc
prefix :: Token -> (Expr -> Expr) -> Operator (Tagged Token) () Expr
prefix tok fun =
    Prefix (do tokenP (==tok); return fun)

exprP :: TokParser Expr
exprP = buildExpressionParser opTable primXP

-- STATEMENTS

letSP :: TokParser Statement
letSP =
    do skip0or1 (tokenP (==LetTok))
       v <- varP
       tokenP (==EqTok)
       x <- exprP
       return (LetS v x)

gotoSP :: TokParser Statement
gotoSP =
    do tokenP (==GoTok)
       tokenP (==ToTok)
       n <- lineNumP
       return (GotoS n)

gosubSP :: TokParser Statement
gosubSP =
    do tokenP (==GoTok)
       tokenP (==SubTok)
       n <- lineNumP
       return (GosubS n)

returnSP :: TokParser Statement
returnSP =
    do tokenP (==ReturnTok)
       return ReturnS

ifSP :: TokParser Statement
ifSP =
    do tokenP (==IfTok)
       x <- exprP
       tokenP (==ThenTok)
       target <- try ifSPGoto <|> statementListP
       return (IfS x target)

ifSPGoto :: TokParser [Tagged Statement]
ifSPGoto =
    do pos <- getPosition
       n <- lineNumP
       return [Tagged pos (GotoS n)]

forSP :: TokParser Statement
forSP =
    do tokenP (==ForTok)
       v <- simpleVarP
       tokenP (==EqTok)
       x1 <- exprP
       tokenP (==ToTok)
       x2 <- exprP
       x3 <- option (LitX (FloatLit 1)) (tokenP (==StepTok) >> exprP)
       return (ForS v x1 x2 x3)

-- handles a NEXT and an optional variable list
nextSP :: TokParser Statement
nextSP = do
    tokenP (==NextTok)
    vs <- sepBy simpleVarP (tokenP (==CommaTok))
    if length vs > 0
        then return (NextS (Just vs))
        else return (NextS Nothing)

printSP :: TokParser Statement
printSP =
    do tokenP (==PrintTok)
       xs <- option [] printSPExprs
       (tokenP (==SemiTok) >> return (PrintS xs False))
           <|> return (PrintS xs True)

optionally :: GenParser tok st a -> GenParser tok st (Maybe a)
optionally p = option Nothing (p >>= return . Just)

printSPExprs :: TokParser [Expr]
printSPExprs =
    do x <- exprP
       xs' <- many (try (optionally (tokenP (==SemiTok)) >> printExprP))
       return (x:xs')

printExprP = nextZoneP <|> exprP

nextZoneP :: TokParser Expr
nextZoneP = do
    tokenP (==CommaTok)
    return NextZoneX

inputSP :: TokParser Statement
inputSP =
    do tokenP (==InputTok)
       ps <- option Nothing inputPrompt
       vs <- sepBy1 varP (tokenP (==CommaTok))
       return (InputS ps vs)

inputPrompt :: TokParser (Maybe String)
inputPrompt =
    do (StringLit p) <- stringLitP
       tokenP (==SemiTok)
       return (Just p)

endSP :: TokParser Statement
endSP =
    do tokenP (==EndTok)
       return EndS

dimSP :: TokParser Statement
dimSP =
    do tokenP (==DimTok)
       arrs <- sepBy1 arrP (tokenP (==CommaTok))
       return (DimS arrs)

randomizeSP :: TokParser Statement
randomizeSP = do
    tok <- tokenP (==RandomizeTok)
    return RandomizeS

remSP :: TokParser Statement
remSP =
    do tok <- tokenP isRemTok
       return (RemS (getRemTokString (getTaggedVal tok)))

statementP :: TokParser (Tagged Statement)
statementP = do input <- getInput
                let pos = getPosTag (head input)
                st <- choice [printSP, inputSP, gotoSP, gosubSP, returnSP,
                    ifSP, forSP, nextSP, endSP, randomizeSP, dimSP, remSP, letSP]
                return (Tagged pos st)

statementListP :: TokParser [Tagged Statement]
statementListP = do
    many (tokenP (==ColonTok))
    sl <- sepEndBy1 statementP (many1 (tokenP (==ColonTok)))
    eof <?> "colon or end of line"
    return sl                    
