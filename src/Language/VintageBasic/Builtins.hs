-- | A representation of BASIC's builtin functions.

module Language.VintageBasic.Builtins where

-- | An enumeration of BASIC's builtin functions.
data Builtin =
    AbsBI | AscBI | AtnBI | ChrBI | CosBI | ExpBI | IntBI | LeftBI | LenBI | LogBI | MidBI
    | RightBI | RndBI | SgnBI | SinBI | SpcBI | StrBI | SqrBI | TabBI | TanBI | ValBI
    deriving (Show,Eq)

-- | An association list mapping BASIC builtins to their string representation.
-- It is used forwards to print BASIC code, and backwards to parse BASIC code.
builtinToStrAssoc :: [(Builtin, String)]
builtinToStrAssoc = [
    (AbsBI,   "ABS"   ),
    (AscBI,   "ASC"   ),
    (AtnBI,   "ATN"   ),
    (ChrBI,   "CHR$"  ),
    (CosBI,   "COS"   ),
    (ExpBI,   "EXP"   ),
    (IntBI,   "INT"   ),
    (LeftBI,  "LEFT$" ),
    (LenBI,   "LEN"   ),
    (LogBI,   "LOG"   ),
    (MidBI,   "MID$"  ),
    (RightBI, "RIGHT$"),
    (RndBI,   "RND"   ),
    (SgnBI,   "SGN"   ),
    (SinBI,   "SIN"   ),
    (SpcBI,   "SPC"   ),
    (StrBI,   "STR$"  ),
    (SqrBI,   "SQR"   ),
    (TabBI,   "TAB"   ),
    (TanBI,   "TAN"   ),
    (ValBI,   "VAL"   )
  ]
