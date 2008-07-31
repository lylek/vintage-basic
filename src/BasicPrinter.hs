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

printVarName :: VarName -> String
printVarName (VarName varType name) = name ++ case varType of
    FloatType  -> ""
    IntType    -> "%"
    StringType -> "$"

printVar :: Var -> String
printVar (ScalarVar vn) = printVarName vn
printVar (ArrVar vn xs) = printVarName vn ++ printArgs xs

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
printExpr (FnX vn xs) = printVarName vn ++ printArgs xs
printExpr (MinusX x) = "-" ++ printExpr x
printExpr (NotX x) = "NOT " ++ printExpr x
printExpr (ParenX x) = "(" ++ printExpr x ++ ")"
printExpr (BinX op x1 x2) = printExpr x1 ++ printOp op ++ printExpr x2
printExpr (BuiltinX b xs) = printBuiltin b ++ printArgs xs

printTaggedStatement :: Tagged Statement -> String
printTaggedStatement (Tagged pos statement) = printStatement statement

printStatement :: Statement -> String
printStatement (LetS v x) = "LET " ++ printVar v ++ "=" ++ printExpr x
printStatement (DimS arrs) =
    "DIM " ++ concat (intersperse "," [printVarName vn ++ printArgs xs | (vn, xs) <- arrs])
printStatement (GotoS label) = "GOTO " ++ show label
printStatement (GosubS label) = "GOSUB " ++ show label
printStatement (OnGotoS x labels) = "ON " ++ printExpr x ++ " GOTO " ++ (concat (intersperse ", " (map show labels)))
printStatement (OnGosubS x labels) = "ON " ++ printExpr x ++ " GOSUB " ++ (concat (intersperse ", " (map show labels)))
printStatement ReturnS = "RETURN"
printStatement (IfS x ss) =
    "IF " ++ printExpr x ++ " THEN " ++ printStatementList ss
printStatement (ForS vn x1 x2 x3) =
    "FOR " ++ printVarName vn ++ "=" ++ printExpr x1 ++ " TO " ++ printExpr x2
       ++ (case x3
           of (LitX (FloatLit 1)) -> ""
              _ -> " STEP " ++ printExpr x3)
printStatement (NextS Nothing) = "NEXT"
printStatement (NextS (Just vns)) =
    "NEXT " ++ (concat $ intersperse "," (map printVarName vns))
printStatement (PrintS xs t) =
    "PRINT " ++ (concat $ intersperse ";" (map printExpr xs))
         ++ (if t then "" else ";")
printStatement (InputS prompt vs) =
    "INPUT " ++ (case prompt of Nothing -> ""; Just ps -> show ps ++ ";")
         ++ (concat $ intersperse "," (map printVar vs))
printStatement EndS = "END"
printStatement RandomizeS = "RANDOMIZE"
printStatement (ReadS vars) = "READ " ++ (concat (intersperse ", " (map printVar vars)))
printStatement (RestoreS Nothing) = "RESTORE"
printStatement (RestoreS (Just n)) = "RESTORE " ++ show n
printStatement (DataS s) = "DATA " ++ s
printStatement (DefFnS vn vns x) =
    "DEF FN" ++ printVarName vn ++ "(" ++ (concat (intersperse ", " (map printVarName vns))) ++ ")"
    ++ " = " ++ printExpr x
printStatement (RemS s) = "REM" ++ s

printStatementList ss = concat $ intersperse ":" (map printTaggedStatement ss)

printLine (Line n ss) = show n ++ " " ++ printStatementList ss ++ "\n"

printLines lines = concat $ map printLine lines
