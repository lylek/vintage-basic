-- BasicSyntax.hs
-- Describes the abstract syntax of BASIC.
-- Lyle Kopnicky

module BasicSyntax where

import BasicBuiltin(Builtin)
import BasicLexCommon(Tagged(..))

type Label = Int

-- TODO: Check if Eq is really necessary for syntax elements

data Literal =
    FloatLit Float
  | StringLit String
    deriving (Show,Eq)

-- Simple variables have an empty expression list, arrays a non-empty one
data Var =
    FloatVar String [Expr]
  | IntVar String [Expr]
  | StringVar String [Expr]
    deriving (Show,Eq)

data BinOp =
    AddOp | SubOp | MulOp | DivOp | PowOp
    | EqOp | NEOp | LTOp | LEOp | GTOp | GEOp
    | AndOp | OrOp
    deriving (Enum,Show,Eq)

data Expr =
    LitX Literal
  | VarX Var
  | FnX Var [Expr]
  | MinusX Expr
  | NotX Expr
  | BinX BinOp Expr Expr
  | BuiltinX Builtin [Expr]
  | NextZoneX
  | ParenX Expr
    deriving (Show,Eq)

data Statement =
    LetS Var Expr
  | DimS [Var]
  | GotoS Label
  | GosubS Label
  | OnGotoS Expr [Label]
  | OnGosubS Expr [Label]
  | ReturnS
  | IfS Expr [Tagged Statement]
  | ForS Var Expr Expr Expr
  | NextS (Maybe [Var])
  | PrintS [Expr] Bool -- True if should print newline
  | InputS (Maybe String) [Var]
  | EndS
  | RandomizeS
  | ReadS [Var]
  | RestoreS (Maybe Label)
  | DataS String
  | DefFnS Var [Var] Expr
  | RemS String
    deriving (Show,Eq)

data Line = Line Label [Tagged Statement]
    deriving (Show,Eq)
