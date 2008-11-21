-- | Results of BASIC computations, including errors.
module BasicResult(BasicResult(..),RuntimeError(..)) where

import Text.ParserCombinators.Parsec(sourceLine,sourceColumn)
import Text.ParserCombinators.Parsec.Error(ParseError,errorMessages,errorPos,showErrorMessages)
import BasicPrinter(printVarName)
import BasicSyntax(Label,VarName)
import DurableTraps(ResultType(..))

data BasicResult =
    Pass
    | ScanError ParseError
    | SyntaxError ParseError
    | RuntimeError Label RuntimeError
    | Next (Maybe String)
    | Return
    | Suspend

instance Show BasicResult where
    show Pass = "NORMAL TERMINATION"
    show (ScanError pe) = showParseError "LINE NUMBERING" "RAW LINE" "END OF FILE" pe
    show (SyntaxError pe) = showParseError "SYNTAX" "LINE" "END OF LINE" pe
    show (RuntimeError label err) = show err ++ " IN LINE " ++ (show label)
    show (Next Nothing) = "!NEXT WITHOUT FOR ERROR"
    show (Next (Just s)) = "!NEXT WITHOUT FOR ERROR (VAR "++s++")"
    show Return = "!RETURN WITHOUT GOSUB ERROR"
    show Suspend = "PROGRAM SUSPENDED"

instance ResultType BasicResult where
    okValue = Pass

data RuntimeError =
    TypeMismatchError
    | InvalidArgumentError
    | DivisionByZeroError
    | BadGotoTargetError Label
    | BadGosubTargetError Label
    | BadRestoreTargetError Label
    | NegativeArrayDimError
    | ReDimensionedArrayError
    | MismatchedArrayDimensionsError
    | OutOfArrayBoundsError
    | UndefinedFunctionError VarName
    | OutOfDataError
  deriving Eq

instance Show RuntimeError where
    show TypeMismatchError = "!TYPE MISMATCH"
    show InvalidArgumentError = "!INVALID ARGUMENT"
    show DivisionByZeroError = "!DIVISION BY ZERO"
    show (BadGotoTargetError lab) = "!BAD GOTO TARGET " ++ show lab
    show (BadGosubTargetError lab) = "!BAD GOSUB TARGET " ++ show lab
    show (BadRestoreTargetError lab) = "!BAD RESTORE TARGET " ++ show lab
    show NegativeArrayDimError = "!NEGATIVE ARRAY DIM"
    show ReDimensionedArrayError = "!REDIM'D ARRAY"
    show MismatchedArrayDimensionsError = "!MISMATCHED ARRAY DIMENSIONS"
    show OutOfArrayBoundsError = "!OUT OF ARRAY BOUNDS"
    show (UndefinedFunctionError vn) = "!UNDEFINED FUNCTION " ++ printVarName vn
    show OutOfDataError = "!OUT OF DATA"

showParseError :: String -> String -> String -> ParseError -> String
showParseError msgErrorType msgLine msgEndOfInput parseError =
    let pos = errorPos parseError
        messages = errorMessages parseError
        line = sourceLine pos
        col = sourceColumn pos
    in
        "!" ++ msgErrorType ++ " ERROR IN " ++ msgLine ++ " " ++ show line
        ++ ", COLUMN " ++ show col
        ++ showErrorMessages "OR" " UNKNOWN" " EXPECTING" " UNEXPECTED" msgEndOfInput messages
