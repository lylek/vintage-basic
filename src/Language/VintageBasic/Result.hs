-- | Results of BASIC computations, including errors.
module Language.VintageBasic.Result(Result(..),RuntimeException(..),RuntimeError(..)) where

import Text.ParserCombinators.Parsec(sourceLine,sourceColumn)
import Text.ParserCombinators.Parsec.Error(ParseError,errorMessages,errorPos,showErrorMessages)
import Language.VintageBasic.Printer(printVarName)
import Language.VintageBasic.Syntax(Label,VarName)
import Control.Monad.CPST.DurableTraps(ResultType(..))

data Result =
    Pass
    | ScanError ParseError
    | SyntaxError ParseError
    | LabeledRuntimeException Label RuntimeException

instance ResultType Result where
    okValue = Pass

data RuntimeException =
    RuntimeError RuntimeError
    | Next (Maybe String)
    | Return
    | Suspend

instance Show Result where
    show Pass = "NORMAL TERMINATION"
    show (ScanError pe) = showParseError "LINE NUMBERING" "RAW LINE" "END OF FILE" pe
    show (SyntaxError pe) = showParseError "SYNTAX" "LINE" "END OF LINE" pe
    show (LabeledRuntimeException label x) = show x ++ " IN LINE " ++ show label

instance Show RuntimeException where
    show (RuntimeError err) = show err
    show (Next Nothing) = "!NEXT WITHOUT FOR ERROR"
    show (Next (Just s)) = "!NEXT WITHOUT FOR ERROR (VAR "++s++")"
    show Return = "!RETURN WITHOUT GOSUB ERROR"
    show Suspend = "!BREAK"

data RuntimeError =
    TypeMismatchError
    | WrongNumberOfArgumentsError
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
    | EndOfInputError
  deriving Eq

instance Show RuntimeError where
    show TypeMismatchError = "!TYPE MISMATCH"
    show WrongNumberOfArgumentsError = "!WRONG NUMBER OF ARGUMENTS"
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
    show EndOfInputError = "!END OF INPUT"

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
