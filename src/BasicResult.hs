-- BasicResult.hs
-- The BasicResult type for Basic computations.
-- Lyle Kopnicky

module BasicResult where

import DurableTraps

data BasicResult = Pass | Fail String | Next (Maybe String) | Return | Suspend

instance Show BasicResult where
    show Pass = "NORMAL TERMINATION"
    show (Fail s) = s
    show (Next Nothing) = "!NEXT WITHOUT FOR ERROR"
    show (Next (Just s)) = "!NEXT WITHOUT FOR ERROR (VAR "++s++")"
    show Return = "!RETURN WITHOUT GOSUB ERROR"
    show Suspend = "PROGRAM SUSPENDED"

instance Eq BasicResult where
    Pass == Pass = True
    Pass == _ = False
    (Fail s1) == (Fail s2) = s1 == s2
    (Fail _) == _ = False
    (Next ms1) == (Next ms2) = ms1 == ms2
    (Next _) == _ = False
    Return == Return = True
    Return == _ = False
    Suspend == Suspend = True
    Suspend == _ = False

instance ResultType BasicResult where
    okValue = Pass
