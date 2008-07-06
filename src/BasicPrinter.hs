-- BasicPrinter.hs
-- Prettyprinter for BASIC syntax.
-- Lyle Kopnicky

module BasicPrinter where

import List
import BasicLexCommon
import BasicSyntax
import BasicBuiltin(Builtin,builtinToStrAssoc)
import Data.Maybe(fromJust)

-- If it's a round number, print it as an integer.
printLit :: Literal -> String
printLit (FloatLit v) = let i = floor v :: Integer
                in if fromInteger i == v
                   then show i
                   else show v
printLit (StringLit s) = show s

printVar :: Var -> String
printVar (FloatVar s []) = s
printVar (IntVar s []) = s ++ "%"
printVar (StringVar s []) = s ++ "$"
printVar (FloatVar s args) = s ++ printArgs args
printVar (IntVar s args) = s ++ "%" ++ printArgs args
printVar (StringVar s args) = s ++"$" ++ printArgs args

printArgs :: [Expr] -> String
printArgs xs = "(" ++ concat (intersperse "," (map printExpr xs)) ++ ")"

printOp AddOp = "+"
printOp SubOp = "-"
printOp MulOp = "*"
printOp DivOp = "/"
printOp PowOp = "^"
printOp EqOp = "="
printOp NEOp = "<>"
printOp LTOp = "<"
printOp LEOp = "<="
printOp GTOp = ">"
printOp GEOp = ">="
printOp AndOp = " AND "
printOp OrOp = " OR "

printBuiltin :: Builtin -> String
printBuiltin b = fromJust $ lookup b builtinToStrAssoc

printExpr :: Expr -> String
printExpr (LitX lit) = printLit lit
printExpr (VarX var) = printVar var
printExpr (MinusX x) = "-" ++ printExpr x
printExpr (NotX x) = "NOT " ++ printExpr x
printExpr (ParenX x) = "(" ++ printExpr x ++ ")"
printExpr (BinX op x1 x2) = printExpr x1 ++ printOp op ++ printExpr x2
printExpr (BuiltinX b xs) = printBuiltin b ++ printArgs xs

printTaggedStatement :: Tagged Statement -> String
printTaggedStatement (Tagged pos statement) = printStatement statement

printStatement :: Statement -> String
printStatement (LetS v x) = "LET " ++ printVar v ++ "=" ++ printExpr x
printStatement (GotoS n) = "GOTO " ++ show n
printStatement (GosubS n) = "GOSUB " ++ show n
printStatement ReturnS = "RETURN"
printStatement (IfS x ss) =
    "IF " ++ printExpr x ++ " THEN " ++ printStatementList ss
printStatement (ForS v x1 x2 x3) =
    "FOR " ++ printVar v ++ "=" ++ printExpr x1 ++ " TO " ++ printExpr x2
       ++ (case x3
           of (LitX (FloatLit 1)) -> ""
              _ -> " STEP " ++ printExpr x3)
           
printStatement (NextS Nothing) = "NEXT"
printStatement (NextS (Just vs)) =
    "NEXT " ++ (concat $ intersperse "," (map printVar vs))
printStatement (PrintS xs t) =
    "PRINT " ++ (concat $ intersperse ";" (map printExpr xs))
         ++ (if t then "" else ";")
printStatement (InputS prompt vs) =
    "INPUT " ++ (case prompt of Nothing -> ""; Just ps -> show ps ++ ";")
         ++ (concat $ intersperse "," (map printVar vs))
printStatement EndS = "END"
printStatement (DimS arr) = "DIM " ++ printVar arr
printStatement (RemS s) = "REM" ++ s

printStatementList ss = concat $ intersperse ":" (map printTaggedStatement ss)

printLine (Line n ss) = show n ++ " " ++ printStatementList ss ++ "\n"

printLines lines = concat $ map printLine lines
