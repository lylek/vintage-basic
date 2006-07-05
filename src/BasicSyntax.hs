-- BasicSyntax.hs
-- Describes the abstract syntax of BASIC.
-- Lyle Kopnicky
-- last updated 2005-07-09

module BasicSyntax where

type Label = Int

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

data Builtin = AbsBI Expr | IntBI Expr | RndBI Expr
             | TabBI Expr

data BinOp = AddOp | SubOp | MulOp | DivOp | PowOp
           | EqOp | NEOp | LTOp | LEOp | GTOp | GEOp
           | AndOp | OrOp
           deriving (Enum,Show,Eq)

data Expr =
    LitX Literal
  | VarX Var
  | MinusX Expr
  | NotX Expr
  | BinX BinOp Expr Expr
  | ParenX Expr
    deriving (Show,Eq)

data Statement =
    LetS Var Expr
  | DimS Var
  | GotoS Label
  | GosubS Label
  | ReturnS
  | IfS Expr [Statement]
  | ForS Var Expr Expr Expr
  | NextS (Maybe [Var])
  | PrintS [Expr] Bool -- True if should print newline
  | InputS (Maybe String) [Var]
  | EndS
  | RemS String
    deriving (Show,Eq)

data Line = Line Label [Statement]
          | SyntaxError Label
          deriving (Show,Eq)
