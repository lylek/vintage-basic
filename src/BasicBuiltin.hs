module BasicBuiltin where

data Builtin =
    AbsBI | AscBI | AtnBI | ChrBI | CosBI | ExpBI | IntBI | LeftBI | LenBI | LogBI | MidBI
    | RightBI | RndBI | SgnBI | SinBI | SpcBI | SqrBI | TabBI | TanBI
    deriving (Show,Eq)

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
    (SqrBI,   "SQR"   ),
    (TabBI,   "TAB"   ),
    (TanBI,   "TAN"   )
  ]
