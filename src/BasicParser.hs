-- BasicParser.hs
-- Parses BASIC source code to produce abstract syntax.
-- Also used at runtime to input values.
-- Lyle Kopnicky
-- last updated 2005-07-09

module BasicParser(linesP,readFloat,dataValsP) where

import Data.Char
import Parser
import SimpleParser
--import LAParser
import BasicSyntax
import BasicTokenizer

type MyParser = SimpleParser

-- only first 2 chars and 1st digit of variable names are siginificant
-- should make this settable by option
varSignifLetters :: Int
varSignifLetters = 2
varSignifDigits :: Int
varSignifDigits = 1

spaceP = maybeP $ isItP (==SpaceTok)

labelP :: MyParser Token Int
labelP = do ds <- manyP' digitP
            return (read (map unTok ds))

-- LITERALS

floatLitP :: MyParser Token Literal
floatLitP = do v <- floatP
	       return (FloatLit v)

intSP :: MyParser Token String
intSP = do ds <- manyP' (isItP (charTokTest isDigit))
	   return (map unTok ds)

sgnP :: MyParser Token String
sgnP = do sgn <- isItP ((==PlusTok)||-(==MinusTok))
	  return (if sgn==PlusTok then "" else "-")

floatP :: MyParser Token Float
floatP = do spaceP
            sgn <- maybeP sgnP
	    mant <- firstOfP (float2P ||| float1P)
	    exp <- maybeP expP
	    return (read (concat sgn++mant++concat exp))

expP :: MyParser Token String
expP = do isItP (==CharTok 'E')
	  esgn <- maybeP sgnP
	  i <- intSP
	  return ("E"++concat esgn++i)

float1P :: MyParser Token String
float1P = intSP

float2P :: MyParser Token String
float2P = do i <- maybeP intSP
	     isItP (==CharTok '.')
	     f <- maybeP intSP
	     return ("0"++concat i++"."++concat f++"0")

stringLitP :: MyParser Token Literal
stringLitP = do (StringTok v) <- isItP isStringTok
		return (StringLit v)

litP = firstOfP (floatLitP ||| stringLitP)

-- VARIABLES

alphaP = isItP (charTokTest isUpper)
digitP = isItP (charTokTest isDigit)

varBaseP = do cs <- manyP' alphaP
	      ds <- manyP digitP
	      return (map unTok (take varSignifLetters cs
				 ++ take varSignifDigits ds))

floatVarP = do name <- varBaseP
	       return (FloatVar name [])

intVarP = do name <- varBaseP
             isItP (==PercentTok)
             return (IntVar name [])

stringVarP = do name <- varBaseP
		isItP (==DollarTok)
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

argsP = do isItP (==LParenTok)
	   spaceP
           x <- exprP
	   spaceP
           xs <- manyP (isItP (==CommaTok) >> spaceP >> exprP)
	   spaceP
           isItP (==RParenTok)
           return (x:xs)

parenXP = do isItP (==LParenTok)
	     spaceP
	     x <- exprP
	     spaceP
	     isItP (==RParenTok)
             return (ParenX x)

plusXP = do isItP (==PlusTok)
	    spaceP
	    exprP

minusXP = do isItP (==MinusTok)
	     spaceP
	     x <- exprP
	     return (MinusX x)

notXP = do isItP (==NotTok)
           spaceP
           x <- exprP
           return (NotX x)

primXP = firstOfP
         (litXP ||| varXP ||| parenXP ||| plusXP ||| minusXP)

-- For a binary expression 'x1 op x2',
-- this parses the 'op x2' tail, making a function that takes
-- 'x1' to the whole expression.  It's much faster this way.
binXP' :: Token -> BinOp -> MyParser Token (Expr -> Expr)
binXP' op tag = do spaceP
		   isItP (==op)
                   spaceP
		   x2 <- exprP
		   return (\x1 -> BinX tag x1 x2)

-- All possible 'tails' of binary expressions.
binXPs' :: MyParser Token (Expr -> Expr)
binXPs' = firstOfP (foldr1 (|||) [binXP' op tag | (op,tag) <- binOps])

-- Careful about the order, e.g. '<=' must precede '<' or it will
-- never get parsed.
binOps = [(PlusTok, AddOp),
	  (MinusTok, SubOp),
	  (MulTok, MulOp),
	  (DivTok, DivOp),
          (PowTok, PowOp),
	  (EqTok, EqOp),
	  (NETok, NEOp),
	  (LETok, LEOp),
	  (LTTok, LTOp),
	  (GETok, GEOp),
	  (GTTok, GTOp),
          (AndTok, AndOp),
          (OrTok, OrOp)
	 ]

exprP :: MyParser Token Expr
exprP = do x1 <- primXP
	   fs <- manyP binXPs'
	   return (fixPrec (foldl (flip ($)) x1 fs))

-- Note: All unary operators bind at higher precedence than binary operators.

precOf :: BinOp -> Int
precOf OrOp   = 0
precOf AndOp  = 1
precOf EqOp   = 2
precOf NEOp   = 2
precOf LEOp   = 2
precOf LTOp   = 2
precOf GEOp   = 2
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

letSP = do maybeP (isItP (==LetTok))
	   spaceP
	   v <- varP
	   spaceP
	   isItP (==EqTok)
	   spaceP
	   x <- exprP
	   return (LetS v x)

gotoSP = do isItP (==GoTok)
	    spaceP
	    isItP (==ToTok)
	    spaceP
	    n <- labelP
	    return (GotoS n)

gosubSP = do isItP (==GoTok)
	     spaceP
	     isItP (==SubTok)
	     spaceP
	     n <- labelP
	     return (GosubS n)

returnSP = do isItP (==ReturnTok)
	      return ReturnS

ifSP = do isItP (==IfTok)
	  spaceP
	  x <- exprP
	  spaceP
	  isItP (==ThenTok)
	  spaceP
	  target <- firstOfP (ifSPGoto ||| statementListP)
	  return (IfS x target)

ifSPGoto = do n <- labelP
	      return [GotoS n]

forSP = do isItP (==ForTok)
	   spaceP
	   v <- simpleVarP
	   spaceP
	   isItP (==EqTok)
	   spaceP
	   x1 <- exprP
	   spaceP
	   isItP (==ToTok)
	   spaceP
	   x2 <- exprP
	   spaceP
	   mx3 <- maybeP (isItP (==StepTok) >> spaceP >> exprP)
	   let x3 = case mx3 of [] -> (LitX (FloatLit 1))
				[step] -> step
	   return (ForS v x1 x2 x3)

-- handles a NEXT and an optional variable list
nextSP = do isItP (==NextTok)
	    spaceP
	    v <- maybeP simpleVarP
	    if length v > 0
	       then do vs <- manyP (spaceP >> isItP (==CommaTok) >> spaceP
				    >> simpleVarP)
		       return (NextS (Just (v++vs)))
	       else return (NextS Nothing)

printSP = do isItP (==PrintTok)
	     spaceP
	     x <- maybeP exprP
	     xs <- manyP printSP'
	     spaceP
	     (isItP (==SemiTok) >> return (PrintS (x++xs) False))
	      ||| return (PrintS (x++xs) True)

printSP' = do spaceP
	      manyP (isItP (==SemiTok))
	      spaceP
	      exprP

inputSP = do isItP (==InputTok)
	     spaceP
	     prompt <- maybeP inputPrompt
	     let ps = case prompt
		      of [] -> Nothing
			 [ps] -> Just ps
	     spaceP
	     v <- varP
	     vs <- manyP (spaceP >> isItP (==CommaTok) >> spaceP >> varP)
	     return (InputS ps (v:vs))

inputPrompt = do (StringTok prompt) <- isItP isStringTok
		 spaceP
		 isItP (==SemiTok)
		 return prompt

endSP = do isItP (==EndTok)
	   return EndS

dimSP = do isItP (==DimTok)
	   spaceP
           arr <- arrP
           return (DimS arr)

remSP = do (RemTok s) <- isItP isRemTok
	   return (RemS s)

statementP = do firstOfP (printSP ||| inputSP
		          ||| gotoSP ||| gosubSP ||| returnSP
		          ||| ifSP ||| forSP ||| nextSP
		          ||| endSP ||| dimSP ||| remSP
                          ||| letSP)

statementListP = do manyP (isItP (==ColonTok) >> spaceP)
		    s <- statementP
		    ss <- manyP statementP'
		    manyP (spaceP >> isItP (==ColonTok))
		    return (s:ss)

statementP' = do manyP' (spaceP >> isItP (==ColonTok))
		 spaceP
		 statementP

-- LINES

lineP = do spaceP
           n <- labelP
	   spaceP
	   ss <- firstOfP (statementListP ||| return [])
	   spaceP
	   firstOfP (lineEndGood n ss ||| lineEndBad n)

lineEndGood n ss = do manyP' (isItP (==EOLTok))
		      return (Line n ss)

lineEndBad n = do manyP (isItP (/=EOLTok))
		  manyP' (isItP (==EOLTok))
		  return (SyntaxError n)

linesP = manyP lineP

-- DATA STATEMENTS / INPUT BUFFER

-- We don't need to look for EOL characters, because these will only be
-- fed single lines.

readFloat :: String -> Maybe Float
readFloat s =
    case floatP $$ (map CharTok s)
	 of [(fv,[])] -> Just fv
	    _ -> Nothing

nonCommaP :: MyParser Char Char
nonCommaP = isItP (/=',')

stringP :: MyParser Char String
stringP = do keyP "\""
	     s <- manyP (isItP (/='\"'))
	     keyP "\""
	     return s

trim :: String -> String
trim s = dropWhile (==' ') $ reverse $ dropWhile (==' ') $ reverse s

dataValP :: MyParser Char String
dataValP = do s <- firstOfP (stringP ||| manyP nonCommaP)
	      return (trim s)

dataValsP :: MyParser Char [String]
dataValsP = do v <- dataValP
	       vs <- manyP (keyP "," >> dataValP)
	       return (v:vs)
