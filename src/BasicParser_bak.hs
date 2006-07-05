-- BasicParser.hs
-- Parses BASIC source code to produce abstract syntax.
-- Also used at runtime to input values.
-- Lyle Kopnicky
-- last updated 2005-07-08

module BasicParser where

import Data.Char
import Parser
import SimpleParser
import LAParser
import BasicSyntax

type MyParser = LAParser SimpleParser

-- only first 2 chars and 1st digit of variable names are siginificant
-- should make this settable by option
varSignifLetters :: Int
varSignifLetters = 2
varSignifDigits :: Int
varSignifDigits = 1

isEOL c = c `elem` "\n\r"

spaceP = manyP (isitP (==' '))
keywP s = do spaceP; keyP s

labelP :: MyParser Char Int
labelP = do spaceP
            ds <- manyP' digitP
            return (read ds)

-- LITERALS

floatLitP :: MyParser Char Literal
floatLitP = do v <- floatP
	       return (FloatLit v)

intSP :: MyParser Char String
intSP = spaceP >> manyP' (isitP isDigit)

sgnP :: MyParser Char String
sgnP = do sgn <- isitP ((=='+')||-(=='-'))
	  return (if sgn=='+' then "" else "-")

floatP :: MyParser Char Float
floatP = do spaceP
            sgn <- maybeP sgnP
	    mant <- firstOfP (float2P ||| float1P)
	    exp <- maybeP expP
	    return (read (concat sgn++mant++concat exp))

expP :: MyParser Char String
expP = do keywP "E"
	  esgn <- maybeP sgnP
	  i <- intSP
	  return ("E"++concat esgn++i)

float1P :: MyParser Char String
float1P = intSP

float2P :: MyParser Char String
float2P = do i <- maybeP intSP
	     keywP "."
	     f <- maybeP intSP
	     return ("0"++concat i++"."++concat f++"0")

stringLitP :: MyParser Char Literal
stringLitP = do v <- stringP
		return (StringLit v)

-- no special escape chars allowed
stringP :: MyParser Char String
stringP = do spaceP
	     keywP "\""
	     s <- manyP (isitP ((/='\n')&&-(/='\"')))
	     keywP "\""
	     return s

litP = firstOfP (floatLitP ||| stringLitP)

-- VARIABLES

alphaP = isitP isUpper
digitP = isitP isDigit
--alphaNumP = elemP (isUpper ||- isDigit)

varBaseP = do cs <- manyP' alphaP
	      ds <- manyP digitP
	      return (take varSignifLetters cs
                      ++ take varSignifDigits ds)

floatVarP = do name <- varBaseP
	       return (FloatVar name [])

intVarP = do name <- varBaseP
             isitP (=='%')
             return (IntVar name [])

stringVarP = do name <- varBaseP
		isitP (=='$')
		return (StringVar name [])

-- Look for string and int vars first because of $ and % suffixes.
simpleVarP = do spaceP; firstOfP (stringVarP ||| intVarP ||| floatVarP)

arrP = do v <- simpleVarP
          xs <- argsP
          return (case v
                    of FloatVar name [] -> FloatVar name xs
                       IntVar name [] -> IntVar name xs
                       StringVar name [] -> StringVar name xs)

varP = firstOfP (arrP ||| simpleVarP)

-- EXPRESSIONS

litXP = do v <- litP
	   return (LitX v)

varXP = do v <- varP
	   return (VarX v)

argsP = do spaceP
           keywP "("
           x <- exprP
           xs <- manyP (keywP "," >> exprP)
           keywP ")"
           return (x:xs)

parenXP = do spaceP; isitP (=='(')
	     x <- exprP
	     spaceP; isitP (==')')
             return (ParenX x)

plusXP = do spaceP
            keywP "+"
	    exprP

minusXP = do spaceP
             keywP "-"
	     x <- exprP
	     return (MinusX x)

notXP = do spaceP
           keywP "NOT"
           spaceP
           x <- exprP
           return (NotX x)

primXP = firstOfP
         (litXP ||| varXP ||| parenXP ||| plusXP ||| minusXP)

-- For a binary expression 'x1 op x2',
-- this parses the 'op x2' tail, making a function that takes
-- 'x1' to the whole expression.  It's much faster this way.
binXP' :: String -> BinOp -> MyParser Char (Expr -> Expr)
binXP' op tag = do spaceP
                   keywP op
                   spaceP
		   x2 <- exprP
		   return (\x1 -> BinX tag x1 x2)

-- All possible 'tails' of binary expressions.
binXPs' :: MyParser Char (Expr -> Expr)
binXPs' = firstOfP (foldr1 (|||) [binXP' op tag | (op,tag) <- binOps])

-- Careful about the order, e.g. '<=' must precede '<' or it will
-- never get parsed.
binOps = [("+", AddOp),
	  ("-", SubOp),
	  ("*", MulOp),
	  ("/", DivOp),
          ("^", PowOp),
	  ("=", EqOp),
	  ("<>",NEOp),
	  ("<=", LTEqOp),
	  ("<", LTOp),
	  (">=", GTEqOp),
	  (">", GTOp),
          ("AND", AndOp),
          ("OR", OrOp)
	 ]

exprP :: MyParser Char Expr
exprP = do x1 <- primXP
	   fs <- manyP binXPs'
	   return (fixPrec (foldl (flip ($)) x1 fs))

-- Note: All unary operators bind at higher precedence than binary operators.

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
precOf PowOp  = 5

-- Fixes the expression for correct operator precedence.
-- All operators are treated as left-associative.
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

letSP = do maybeP (keywP "LET")
	   v <- varP
	   keywP "="
	   x <- exprP
	   return (LetS v x)

gotoSP = do keywP "GOTO"
	    n <- labelP
	    return (GotoS n)

gosubSP = do keywP "GOSUB"
	     n <- labelP
	     return (GosubS n)

returnSP = do keywP "RETURN"
	      return ReturnS

ifSP = do keywP "IF"
	  x <- exprP
	  keywP "THEN"
	  target <- firstOfP (ifSPGoto ||| statementListP)
	  return (IfS x target)

ifSPGoto = do n <- labelP
	      return [GotoS n]

forSP = do keywP "FOR"
	   v <- simpleVarP
	   keywP "="
	   x1 <- exprP
	   keywP "TO"
	   x2 <- exprP
	   mx3 <- maybeP (do keywP "STEP"; exprP)
	   let x3 = case mx3 of [] -> (LitX (FloatLit 1))
				[step] -> step
	   return (ForS v x1 x2 x3)

-- handles a NEXT and an optional variable list
nextSP = do keywP "NEXT"
	    v <- maybeP simpleVarP
	    if length v > 0
	       then do vs <- manyP (keywP "," >> simpleVarP)
		       return (NextS (Just (v++vs)))
	       else return (NextS Nothing)

printSP = do (keywP "?" ||| keywP "PRINT")
	     x <- maybeP exprP
	     xs <- manyP printSP'
	     ((keywP ";" >> return (PrintS (x++xs) False))
	      ||| return (PrintS (x++xs) True))

printSP' = do manyP (keywP ";")
	      exprP

inputSP = do keywP "INPUT"
	     prompt <- maybeP inputPrompt
	     let ps = case prompt
		      of [] -> Nothing
			 [ps] -> Just ps
	     v <- varP
	     vs <- manyP (keywP "," >> varP)
	     return (InputS ps (v:vs))

inputPrompt = do prompt <- stringP
		 keywP ";"
		 return prompt

endSP = do keywP "END"
	   return EndS

dimSP = do keywP "DIM"
           arr <- arrP
           return (DimS arr)

remSP = do keywP "REM"
	   s <- manyP (isitP (not.isEOL))
	   return (RemS s)

statementP = do spaceP
                firstOfP (printSP ||| inputSP
		          ||| gotoSP ||| gosubSP ||| returnSP
		          ||| ifSP ||| forSP ||| nextSP
		          ||| endSP ||| dimSP ||| remSP
                          ||| letSP)

statementListP = do manyP (keywP ":")
		    s <- statementP
		    ss <- manyP statementP'
		    manyP (keywP ":")
		    return (s:ss)

statementP' = do manyP' (keywP ":")
		 statementP

-- LINES

lineP = do spaceP
           n <- labelP
	   ss <- firstOfP (statementListP ||| return [])
	   spaceP
	   firstOfP (lineEndGood n ss ||| lineEndBad n)

lineEndGood n ss = do manyP' (isitP isEOL)
		      return (Line n ss)

lineEndBad n = do manyP (isitP (not.isEOL))
		  manyP' (isitP isEOL)
		  return (SyntaxError n)

linesP = manyP lineP

-- DATA STATEMENTS / INPUT BUFFER

nonCommaP = isitP ((not.isEOL)&&-(/=','))

trim s = dropWhile (==' ') $ reverse $ dropWhile (==' ') $ reverse s

dataValP = do s <- firstOfP (stringP ||| manyP nonCommaP)
	      return (trim s)

dataValsP = do v <- dataValP
	       vs <- manyP (keyP "," >> dataValP)
	       return (v:vs)
