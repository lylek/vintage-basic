-- Result.hs
-- The Result type for Basic computations.
-- Lyle Kopnicky
-- last updated 2004-09-21

module Result where

import DurableTraps

data Result = Pass | Fail String | Next (Maybe String) | Return | Suspend

instance Show Result where
    show Pass = "NORMAL TERMINATION"
    show (Fail s) = s
    show (Next Nothing) = "!NEXT WITHOUT FOR ERROR"
    show (Next (Just s)) = "!NEXT WITHOUT FOR ERROR (VAR "++s++")"
    show Return = "!RETURN WITHOUT GOSUB ERROR"
    show Suspend = "PROGRAM SUSPENDED"

instance Eq Result where
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

instance ResultType Result where
    okValue = Pass
