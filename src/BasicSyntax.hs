-- BasicSyntax.hs
-- Describes the abstract syntax of 
-- Lyle Kopnicky

module BasicSyntax where

import BasicBuiltin(Builtin)
import BasicLexCommon(Tagged(..))

type Label = Int

-- TODO: Check if Eq is really necessary for syntax elements

data ValType = FloatType | IntType | StringType
    deriving (Show,Eq)

class Typeable a where
    typeOf :: a -> ValType

data Literal =
    FloatLit Float
  | StringLit String
    deriving (Show,Eq)

instance Typeable Literal where
    typeOf (FloatLit  _) = FloatType
    typeOf (StringLit _) = StringType

data VarName = VarName ValType String
    deriving (Show,Eq)

instance Typeable VarName where
    typeOf (VarName valType _) = valType

data Var = ScalarVar VarName | ArrVar VarName [Expr]
    deriving (Show, Eq)

instance Typeable Var where
    typeOf (ScalarVar varName) = typeOf varName
    typeOf (ArrVar varName _)  = typeOf varName

data BinOp =
    AddOp | SubOp | MulOp | DivOp | PowOp
    | EqOp | NEOp | LTOp | LEOp | GTOp | GEOp
    | AndOp | OrOp
    deriving (Enum,Show,Eq)

data Expr =
    LitX Literal
  | VarX Var
  | FnX VarName [Expr]
  | MinusX Expr
  | NotX Expr
  | BinX BinOp Expr Expr
  | BuiltinX Builtin [Expr]
  | NextZoneX
  | ParenX Expr
    deriving (Show,Eq)

data Statement =
    LetS Var Expr
  | DimS [(VarName, [Expr])]
  | GotoS Label
  | GosubS Label
  | OnGotoS Expr [Label]
  | OnGosubS Expr [Label]
  | ReturnS
  | IfS Expr [Tagged Statement]
  | ForS VarName Expr Expr Expr
  | NextS (Maybe [VarName])
  | PrintS [Expr] Bool -- True if should print newline
  | InputS (Maybe String) [Var]
  | EndS
  | RandomizeS
  | ReadS [Var]
  | RestoreS (Maybe Label)
  | DataS String
  | DefFnS VarName [VarName] Expr
  | RemS String
    deriving (Show,Eq)

data Line = Line Label [Tagged Statement]
    deriving (Show,Eq)
