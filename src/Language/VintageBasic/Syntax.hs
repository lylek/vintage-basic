{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

-- | Describes the abstract syntax of BASIC.

module Language.VintageBasic.Syntax where

import Language.VintageBasic.Builtins(Builtin)
import Language.VintageBasic.LexCommon(Tagged(..))
import GHC.Generics (Generic)
import Data.Hashable

-- | A BASIC line number.
type Label = Int

-- | BASIC value types. The IntType is used for storage or function arguments
-- but is always converted to FloatType for use in expressions.
data ValType = FloatType | IntType | StringType
    deriving (Show,Eq,Generic,Hashable)

class Typeable a where
    typeOf :: a -> ValType

instance Typeable ValType where
    typeOf = id

data Literal =
    FloatLit Float
  | StringLit String
    deriving (Show,Eq)

instance Typeable Literal where
    typeOf (FloatLit  _) = FloatType
    typeOf (StringLit _) = StringType

data VarName = VarName ValType String
    deriving (Show,Eq,Generic,Hashable)

instance Typeable VarName where
    typeOf (VarName valType _) = valType

data Var = ScalarVar VarName | ArrVar VarName [Expr]
    deriving (Show,Eq)

instance Typeable Var where
    typeOf (ScalarVar varName) = typeOf varName
    typeOf (ArrVar varName _)  = typeOf varName

-- | BASIC binary operators.
data BinOp =
    AddOp | SubOp | MulOp | DivOp | PowOp
    | EqOp | NEOp | LTOp | LEOp | GTOp | GEOp
    | AndOp | OrOp
    deriving (Enum,Show,Eq)

-- | BASIC expressions.
data Expr =
    LitX Literal
  | VarX Var
  | FnX VarName [Expr]
  | MinusX Expr
  | NotX Expr
  | BinX BinOp Expr Expr
  | BuiltinX Builtin [Expr]
  | NextZoneX                -- ^ commas in a @PRINT@ statement
  | EmptySeparatorX          -- ^ semicolons in a @PRINT@ statement
  | ParenX Expr
    deriving (Show,Eq)

isPrintSeparator :: Expr -> Bool
isPrintSeparator NextZoneX = True
isPrintSeparator EmptySeparatorX = True
isPrintSeparator _ = False

-- | BASIC statements.
data Statement =
    LetS Var Expr
  | DimS [(VarName, [Expr])]
  | GotoS Label
  | GosubS Label
  | OnGotoS Expr [Label]
  | OnGosubS Expr [Label]
  | ReturnS
  | IfS Expr [Tagged Statement] -- ^ includes all statements on the line following the @IF@
  | ForS VarName Expr Expr Expr
  | NextS (Maybe [VarName])
  | PrintS [Expr]
  | InputS (Maybe String) [Var]
  | EndS
  | StopS
  | RandomizeS
  | ReadS [Var]
  | RestoreS (Maybe Label)
  | DataS String
  | DefFnS VarName [VarName] Expr
  | RemS String
    deriving (Show,Eq)

-- | A line of BASIC, in fully parsed form, ready for interpretation.
data Line = Line Label [Tagged Statement]
    deriving (Show)
