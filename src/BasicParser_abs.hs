-- BasicParser.hs
-- Parses BASIC source code to produce abstract syntax.
-- Also used at runtime to input values.
-- Lyle Kopnicky
-- last updated 2005-07-05

module BasicParser where

import Data.Char
import Parser
import BasicSyntax

-- TO DO: Write a tokenizer to come before parser, check where spaces permitted

-- only first 2 chars and 1st digit of variable names are siginificant
-- should make this settable by option
varSignifLetters :: Int
varSignifLetters = 2
varSignifDigits :: Int
varSignifDigits = 1

isEOL c = c `elem` "\n\r"

spaceP :: Parser t Char => t Char String
spaceP = manyP (isitP (==' '))
keywP :: Parser t Char => String -> t Char String
keywP s = do spaceP; keyP s

labelP :: Parser t Char => t Char Int
labelP = do spaceP
            ds <- manyP' digitP
            return (read ds)

-- LITERALS

floatLitP :: Parser t Char => t Char Literal
floatLitP = do v <- floatP
	       return (FloatLit v)

intSP :: Parser t Char => t Char String
intSP = spaceP >> manyP' (isitP isDigit)

sgnP :: Parser t Char => t Char String
sgnP = do sgn <- isitP ((=='+')||-(=='-'))
	  return (if sgn=='+' then "" else "-")

floatP :: Parser t Char => t Char Float
floatP = do spaceP
            sgn <- maybeP sgnP
	    mant <- firstOfP (float2P ||| float1P)
	    exp <- maybeP expP
	    return (read (concat sgn++mant++concat exp))

expP :: Parser t Char => t Char String
expP = do keywP "E"
	  esgn <- maybeP sgnP
	  i <- intSP
	  return ("E"++concat esgn++i)

float1P :: Parser t Char => t Char String
float1P = intSP

float2P :: Parser t Char => t Char String
float2P = do i <- maybeP intSP
	     keywP "."
	     f <- maybeP intSP
	     return ("0"++concat i++"."++concat f++"0")

stringLitP :: Parser t Char => t Char Literal
stringLitP = do v <- stringP
		return (StringLit v)

-- no special escape chars allowed
stringP :: Parser t Char => t Char String
stringP = do spaceP
	     keywP "\""
	     s <- manyP (isitP ((/='\n')&&-(/='\"')))
	     keywP "\""
	     return s

litP :: Parser t Char => t Char Literal
litP = firstOfP (floatLitP ||| stringLitP)

-- VARIABLES

alphaP :: Parser t Char => t Char Char
alphaP = isitP isUpper
digitP :: Parser t Char => t Char Char
digitP = isitP isDigit
--alphaNumP = elemP (isUpper ||- isDigit)

varBaseP :: Parser t Char => t Char String
varBaseP = do cs <- manyP' alphaP
	      ds <- manyP digitP
	      return (take varSignifLetters cs
                      ++ take varSignifDigits ds)

floatVarP :: Parser t Char => t Char Var
floatVarP = do name <- varBaseP
	       return (FloatVar name)

intVarP :: Parser t Char => t Char Var
intVarP = do name <- varBaseP
             isitP (=='%')
             return (IntVar name)

stringVarP :: Parser t Char => t Char Var
stringVarP = do name <- varBaseP
		isitP (=='$')
		return (StringVar name)

-- Look for string and int vars first because of $ and % suffixes.
varP :: Parser t Char => t Char Var
varP = do spaceP; firstOfP (stringVarP ||| intVarP ||| floatVarP)

arrP :: Parser t Char => t Char Arr
arrP = do v <- varP
          xs <- argsP
          return (case v
                    of FloatVar name -> FloatArr name xs
                       IntVar name -> IntArr name xs
                       StringVar name -> StringArr name xs)

-- EXPRESSIONS

litXP :: Parser t Char => t Char Expr
litXP = do v <- litP
	   return (LitX v)

varXP :: Parser t Char => t Char Expr
varXP = do v <- varP
	   return (VarX v)

argsP :: Parser t Char => t Char [Expr]
argsP = do spaceP
           keywP "("
           x <- exprP
           xs <- manyP (keywP "," >> exprP)
           keywP ")"
           return (x:xs)

arrXP :: Parser t Char => t Char Expr
arrXP = do arr <- arrP
           return (ArrX arr)

parenXP :: Parser t Char => t Char Expr
parenXP = do spaceP; isitP (=='(')
	     x <- exprP
	     spaceP; isitP (==')')
             return (ParenX x)

plusXP :: Parser t Char => t Char Expr
plusXP = do spaceP
            keywP "+"
	    exprP

minusXP :: Parser t Char => t Char Expr
minusXP = do spaceP
             keywP "-"
	     x <- exprP
	     return (MinusX x)

notXP :: Parser t Char => t Char Expr
notXP = do spaceP
           keywP "NOT"
           spaceP
           x <- exprP
           return (NotX x)

primXP :: Parser t Char => t Char Expr
primXP = firstOfP
         (litXP ||| arrXP ||| varXP ||| parenXP ||| plusXP ||| minusXP)

-- For a binary expression 'x1 op x2',
-- this parses the 'op x2' tail, making a function that takes
-- 'x1' to the whole expression.  It's much faster this way.
binXP' :: Parser t Char => String -> BinOp -> t Char (Expr -> Expr)
binXP' op tag = do spaceP
                   keywP op
                   spaceP
		   x2 <- exprP
		   return (\x1 -> BinX tag x1 x2)

-- All possible 'tails' of binary expressions.
binXPs' :: Parser t Char => t Char (Expr -> Expr)
binXPs' = firstOfP (foldr1 (|||) [binXP' op tag | (op,tag) <- binOps])

-- Careful about the order, e.g. '<=' must precede '<' or it will
-- never get parsed.
binOps = [("+", AddOp),
	  ("-", SubOp),
	  ("*", MulOp),
	  ("/", DivOp),
	  ("=", EqOp),
	  ("<>",NEOp),
	  ("<=", LTEqOp),
	  ("<", LTOp),
	  (">=", GTEqOp),
	  (">", GTOp),
          ("AND", AndOp),
          ("OR", OrOp)
	 ]

exprP :: Parser t Char => t Char Expr
exprP = do x1 <- primXP
	   fs <- manyP binXPs'
	   return (fixPrec (foldl (flip ($)) x1 fs))

-- TO DO: Consider the precedence of unary operators relative to binary ones

precOf :: BinOp -> Int
precOf OrOp   = 0
precOf AndOp  = 1
precOf EqOp   = 2
precOf NEOp   = 2
precOf LTEqOp = 2
precOf LTOp   = 2
precOf GTEqOp = 2
precOf GTOp   = 2
precOf AddOp  = 3
precOf SubOp  = 3
precOf MulOp  = 4
precOf DivOp  = 4

-- Fixes the expression for correct operator precedence.
fixPrec :: Expr -> Expr
fixPrec (BinX op1 x1 x2) = bubblePrec (BinX op1 (fixPrec x1) (fixPrec x2))
fixPrec x = x

-- Checks for a precedence imbalance at the top level of the expression,
-- and propagates a correction down the tree.  All binary operators are
-- left-associative, so in the case of equal precedence, the operator on
-- the right is considered lower priority.
bubblePrec :: Expr -> Expr
bubblePrec x@(BinX op2 (BinX op1 x1 x2) (BinX op3 x3 x4)) =
    if precOf op1 < precOf op2
       then BinX op1 x1 (bubblePrec (BinX op2 x2 (BinX op3 x3 x4)))
       else if precOf op3 <= precOf op2
	    then BinX op3 (bubblePrec (BinX op2 (BinX op1 x1 x2) x3)) x4
	    else x
bubblePrec x@(BinX op2 (BinX op1 x1 x2) xb) =
    if precOf op1 < precOf op2
       then BinX op1 x1 (bubblePrec (BinX op2 x2 xb))
       else x
bubblePrec x@(BinX op2 xa (BinX op3 x3 x4)) =
    if precOf op3 <= precOf op2
       then BinX op3 (bubblePrec (BinX op2 xa x3)) x4
       else x
bubblePrec x = x

-- STATEMENTS

letSP :: Parser t Char => t Char Statement
letSP = do maybeP (keywP "LET")
	   v <- varP
	   keywP "="
	   x <- exprP
	   return (LetS v x)

letArrSP :: Parser t Char => t Char Statement
letArrSP = do maybeP (keywP "LET")
              arr <- arrP
              keywP "="
              x <- exprP
              return (LetArrS arr x)

gotoSP :: Parser t Char => t Char Statement
gotoSP = do keywP "GOTO"
	    n <- labelP
	    return (GotoS n)

gosubSP :: Parser t Char => t Char Statement
gosubSP = do keywP "GOSUB"
	     n <- labelP
	     return (GosubS n)

returnSP :: Parser t Char => t Char Statement
returnSP = do keywP "RETURN"
	      return ReturnS

ifSP :: Parser t Char => t Char Statement
ifSP = do keywP "IF"
	  x <- exprP
	  keywP "THEN"
	  target <- firstOfP (ifSPGoto ||| statementListP)
	  return (IfS x target)

ifSPGoto :: Parser t Char => t Char [Statement]
ifSPGoto = do n <- labelP
	      return [GotoS n]

forSP :: Parser t Char => t Char Statement
forSP = do keywP "FOR"
	   v <- varP
	   keywP "="
	   x1 <- exprP
	   keywP "TO"
	   x2 <- exprP
	   mx3 <- maybeP (do keywP "STEP"; exprP)
	   let x3 = case mx3 of [] -> (LitX (FloatLit 1))
				[step] -> step
	   return (ForS v x1 x2 x3)

-- handles a NEXT and an optional variable list
nextSP :: Parser t Char => t Char Statement
nextSP = do keywP "NEXT"
	    v <- maybeP varP
	    if length v > 0
	       then do vs <- manyP (keywP "," >> varP)
		       return (NextS (Just (v++vs)))
	       else return (NextS Nothing)

printSP :: Parser t Char => t Char Statement
printSP = do (keywP "?" ||| keywP "PRINT")
	     x <- maybeP exprP
	     xs <- manyP printSP'
	     ((keywP ";" >> return (PrintS (x++xs) False))
	      ||| return (PrintS (x++xs) True))

printSP' :: Parser t Char => t Char Expr
printSP' = do manyP (keywP ";")
	      exprP

inputSP :: Parser t Char => t Char Statement
inputSP = do keywP "INPUT"
	     prompt <- maybeP inputPrompt
	     let ps = case prompt
		      of [] -> Nothing
			 [ps] -> Just ps
	     v <- varP
	     vs <- manyP (keywP "," >> varP)
	     return (InputS ps (v:vs))

inputPrompt :: Parser t Char => t Char String
inputPrompt = do prompt <- stringP
		 keywP ";"
		 return prompt

endSP :: Parser t Char => t Char Statement
endSP = do keywP "END"
	   return EndS

dimSP :: Parser t Char => t Char Statement
dimSP = do keywP "DIM"
           arr <- arrP
           return (DimS arr)

remSP :: Parser t Char => t Char Statement
remSP = do keywP "REM"
	   s <- manyP (isitP (not.isEOL))
	   return (RemS s)

statementP :: Parser t Char => t Char Statement
statementP = do spaceP
                firstOfP (printSP ||| inputSP
		          ||| gotoSP ||| gosubSP ||| returnSP
		          ||| ifSP ||| forSP ||| nextSP
		          ||| endSP ||| dimSP ||| remSP
                          ||| letArrSP ||| letSP)

statementListP :: Parser t Char => t Char [Statement]
statementListP = do manyP (keywP ":")
		    s <- statementP
		    ss <- manyP statementP'
		    manyP (keywP ":")
		    return (s:ss)

statementP' :: Parser t Char => t Char Statement
statementP' = do manyP' (keywP ":")
		 statementP

-- LINES

lineP :: Parser t Char => t Char Line
lineP = do spaceP
           n <- labelP
	   ss <- firstOfP (statementListP ||| return [])
	   spaceP
	   firstOfP (lineEndGood n ss ||| lineEndBad n)

lineEndGood :: Parser t Char => Label -> [Statement] -> t Char Line
lineEndGood n ss = do manyP' (isitP isEOL)
		      return (Line n ss)

lineEndBad :: Parser t Char => Label -> t Char Line
lineEndBad n = do manyP (isitP (not.isEOL))
		  manyP' (isitP isEOL)
		  return (SyntaxError n)

linesP :: Parser t Char => t Char [Line]
linesP = manyP lineP

-- INPUT BUFFER (runtime)

nonCommaP :: Parser t Char => t Char String
nonCommaP = (do c <- isitP ((not.isEOL)&&-(/=',')&&-(/='\"')); return [c])
	    ||| stringP

inputValP :: Parser t Char => t Char String
inputValP = do us <- manyP nonCommaP
	       return (concat us)

inputBufP :: Parser t Char => t Char [String]
inputBufP = do v <- inputValP
	       vs <- manyP (keyP "," >> inputValP)
	       return (v:vs)

forceFloatP :: Parser t Char => t Char Float
forceFloatP = firstOfP (floatP ||| (manyP nonCommaP >> return 0.0))
