module BasicBuiltin where

data Builtin =
    AbsBI | AscBI | AtnBI | ChrBI | CosBI | ExpBI | IntBI | LeftBI | LenBI | LogBI | MidBI
    | RightBI | RndBI | SgnBI | SinBI | SpcBI | StrBI | SqrBI | TabBI | TanBI | ValBI
    deriving (Show,Eq)

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
    (SpcBI,   "SPC"   ),
    (StrBI,   "STR$"  ),
    (SqrBI,   "SQR"   ),
    (TabBI,   "TAB"   ),
    (TanBI,   "TAN"   ),
    (ValBI,   "VAL"   )
  ]
