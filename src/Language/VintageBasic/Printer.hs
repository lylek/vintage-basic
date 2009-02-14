-- | Prettyprinter for BASIC syntax and values.

module Language.VintageBasic.Printer where

import Numeric
import Data.List
import Language.VintageBasic.LexCommon
import Language.VintageBasic.Syntax
import Language.VintageBasic.Builtins(Builtin,builtinToStrAssoc)
import Data.Maybe(fromJust)

-- | Prettyprint a BASIC literal.
printLit :: Literal -> String
printLit (FloatLit v) = let i = floor v :: Integer
                in if fromInteger i == v
                   then show i
                   else show v
printLit (StringLit s) = show s

-- | Prettyprint a floating point value, BASIC style.
printFloat :: Float -> String
printFloat x | x == 0 = " 0"
printFloat x | x < 0  = "-" ++ printPosFloat (-x)
printFloat x | x > 0  = " " ++ printPosFloat x
printFloat _ = error "illegal number for printFloat"

-- | The maximum number of decimal digits needed to display the mantissa of a
-- Float. For IEEE 754-2008 binary32 format, this is 8 decimal digits.
maxFloatDigits :: Int
maxFloatDigits = ceiling (log 2 / log 10 * fromIntegral(floatDigits (0::Float)) :: Float)

printPosFloat :: Float -> String
printPosFloat x =
    let (digits, ex) = floatToDigits 10 x
    in
        if ex <= maxFloatDigits && length digits - ex <= maxFloatDigits
           then
               if ex >= length digits
                   then concatMap show (padDigitsRight digits ex)
                   else
                       if ex > 0
                           then concatMap show (take ex digits) ++ "."
                               ++ concatMap show (drop ex digits)
                           else "." ++ concatMap show (padDigitsLeft digits ex)
           else
               concatMap show (take 1 digits) ++ "."
                   ++ concatMap show (drop 1 digits) ++ "E"
                   ++ (if ex >= 1 then "+" else "") ++ show (ex - 1)

padDigitsRight :: [Int] -> Int -> [Int]
padDigitsRight digits ex = digits ++ replicate (ex - length digits) 0

padDigitsLeft :: [Int] -> Int -> [Int]
padDigitsLeft digits ex = replicate (-ex) 0 ++ digits

-- | Prettyprint a variable name.
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

printOp :: BinOp -> String
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
printExpr NextZoneX = ","
printExpr EmptySeparatorX = ";"
printExpr (NotX x) = "NOT " ++ printExpr x
printExpr (ParenX x) = "(" ++ printExpr x ++ ")"
printExpr (BinX op x1 x2) = printExpr x1 ++ printOp op ++ printExpr x2
printExpr (BuiltinX b xs) = printBuiltin b ++ printArgs xs

printTaggedStatement :: Tagged Statement -> String
printTaggedStatement (Tagged _ statement) = printStatement statement

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
printStatement (PrintS xs) =
    "PRINT " ++ (concat $ intersperse " " (map printExpr xs))
printStatement (InputS prompt vs) =
    "INPUT " ++ (case prompt of Nothing -> ""; Just ps -> show ps ++ ";")
         ++ (concat $ intersperse "," (map printVar vs))
printStatement EndS = "END"
printStatement StopS = "STOP"
printStatement RandomizeS = "RANDOMIZE"
printStatement (ReadS vars) = "READ " ++ (concat (intersperse ", " (map printVar vars)))
printStatement (RestoreS Nothing) = "RESTORE"
printStatement (RestoreS (Just n)) = "RESTORE " ++ show n
printStatement (DataS s) = "DATA " ++ s
printStatement (DefFnS vn vns x) =
    "DEF FN" ++ printVarName vn ++ "(" ++ (concat (intersperse ", " (map printVarName vns))) ++ ")"
    ++ " = " ++ printExpr x
printStatement (RemS s) = "REM" ++ s

printStatementList :: [Tagged Statement] -> String
printStatementList ss = concat $ intersperse ":" (map printTaggedStatement ss)

printLine :: Line -> String
printLine (Line n ss) = show n ++ " " ++ printStatementList ss ++ "\n"

printLines :: [Line] -> String
printLines progLines = concatMap printLine progLines
