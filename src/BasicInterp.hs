-- BasicInterp.hs
-- The heart of the interpreter, which uses the Basic monad.
-- Lyle Kopnicky
-- last updated 2005-07-09

module BasicInterp where

import Maybe
import BasicSyntax
import CPST
import BasicMonad
import ExceptionHandlers
import DurableTraps
import Result
import Parser      -- for INPUT
import BasicParser -- for INPUT
import List

data Val = FloatVal Float | StringVal String | Mismatch | DivByZero
	 deriving (Eq,Show,Ord)

boolToVal :: Bool -> Val
boolToVal t = if t then -1 else 0

isFloat (FloatVal _) = True
isFloat _ = False

isString (StringVal _) = True
isString _ = False

isNext (Next Nothing) = True
isNext _ = False

isNextVar s1 (Next (Just s2)) = s1==s2
isNextVar s1 _ = False

isReturn Return = True
isReturn _ = False

unFV (FloatVal fv) = fv
unSV (StringVal sv) = sv

eval :: Expr -> Basic (BasicExcep Result ()) Val
eval x = do v <- eval' x
            case v of
                   Mismatch -> do raiseCC (Fail "!TYPE MISMATCH IN EXPRESSION")
                                  return v
                   DivByZero -> do raiseCC (Fail "!DIVISON BY ZERO")
                                   return v
                   v -> return v

eval' :: Expr -> Basic (BasicExcep Result ()) Val
eval' (LitX (FloatLit v)) = return (FloatVal v)
eval' (LitX (StringLit v)) = return (StringVal v)
eval' (VarX var) = getVal var
eval' (MinusX x) =
    do val <- eval x
       return (-val)
eval' (NotX x) =
    do val <- eval x
       return (if val==0 then -1 else 0)
eval' (BinX op x1 x2) =
    do v1 <- eval' x1
       v2 <- eval' x2
       if sameType v1 v2
          then evalBinOp op v1 v2
          else return Mismatch
eval' (ParenX x) = eval x

evalBinOp :: BinOp -> Val -> Val -> Basic (BasicExcep Result ()) Val
evalBinOp op v1 v2 =
    case op of
      AddOp -> return (v1+v2)
      SubOp -> return (v1-v2)
      MulOp -> return (v1*v2)
      DivOp -> if v2==0 then return DivByZero
	       else return (v1/v2)
      PowOp -> return (v1**v2)
      EqOp -> return (boolToVal (v1==v2))
      NEOp -> return (boolToVal (v1/=v2))
      LTOp -> return (boolToVal (v1<v2))
      LEOp -> return (boolToVal (v1<=v2))
      GTOp -> return (boolToVal (v1>v2))
      GEOp -> return (boolToVal (v1>=v2))
      AndOp -> return (if v1/=0 && v2/=0 then v1 else 0)
      OrOp -> return (if v1/=0 then v1 else v2)
-- TO DO: check defined behavior of AND & OR

instance Num Val where
    (FloatVal v1) + (FloatVal v2) = FloatVal (v1+v2)
    (StringVal v1) + (StringVal v2) = StringVal (v1++v2)
    _ + _ = Mismatch
    (FloatVal v1) - (FloatVal v2) = FloatVal (v1-v2)
    _ - _ = Mismatch
    (FloatVal v1) * (FloatVal v2) = FloatVal (v1*v2)
    _ * _ = Mismatch
    negate (FloatVal v1) = FloatVal (negate v1)
    negate _ = Mismatch
    abs (FloatVal v1) = FloatVal (abs v1)
    abs _ = Mismatch
    signum (FloatVal v1) = FloatVal (signum v1)
    signum _ = Mismatch
    fromInteger n = FloatVal (fromInteger n)

instance Fractional Val where
    (FloatVal v1) / (FloatVal v2) = FloatVal (v1/v2)
    DivByZero / _ = DivByZero
    _ / DivByZero = DivByZero
    _ / _ = Mismatch
    recip (FloatVal v1) = if v1==0 then DivByZero else FloatVal (recip v1)
    recip DivByZero = DivByZero
    recip _ = Mismatch
    fromRational v = FloatVal (fromRational v)

instance Floating Val where
    pi = FloatVal pi
    exp (FloatVal v) = FloatVal (exp v)
    exp _ = Mismatch
    sqrt (FloatVal v) = FloatVal (sqrt v)
    sqrt _ = Mismatch
    log (FloatVal v) = FloatVal (log v)
    log _ = Mismatch
    (FloatVal v1) ** (FloatVal v2) = FloatVal (v1**v2)
    _ ** _ = Mismatch
    logBase (FloatVal v1) (FloatVal v2) = FloatVal (logBase v1 v2)
    logBase _ _ = Mismatch
    sin (FloatVal v) = FloatVal (sin v)
    sin _ = Mismatch
    tan (FloatVal v) = FloatVal (tan v)
    tan _ = Mismatch
    cos (FloatVal v) = FloatVal (cos v)
    cos _ = Mismatch
    asin (FloatVal v) = FloatVal (asin v)
    asin _ = Mismatch
    atan (FloatVal v) = FloatVal (atan v)
    atan _ = Mismatch
    acos (FloatVal v) = FloatVal (acos v)
    acos _ = Mismatch
    sinh (FloatVal v) = FloatVal (sinh v)
    sinh _ = Mismatch
    tanh (FloatVal v) = FloatVal (tanh v)
    tanh _ = Mismatch
    cosh (FloatVal v) = FloatVal (cosh v)
    cosh _ = Mismatch
    asinh (FloatVal v) = FloatVal (asinh v)
    asinh _ = Mismatch
    atanh (FloatVal v) = FloatVal (atanh v)
    atanh _ = Mismatch
    acosh (FloatVal v) = FloatVal (acosh v)
    acosh _ = Mismatch

sameType :: Val -> Val -> Bool
sameType (FloatVal _) (FloatVal _) = True
sameType (StringVal _) (StringVal _) = True
sameType _ _ = False

-- Interpret a single statement.
-- In the type of interpS, the first () signifies what is passed to a
-- resumed trap.  The second one represents what is returned by interpS.
interpS :: [(Label, Code)] -> Statement -> CodeSeg

interpS _ (RemS s) = return ()

interpS _ EndS = end >> return ()

interpS _ (DimS arr) =
    do let (name, xs) = case arr of
                          (FloatVar name xs) -> (name, xs)
                          (IntVar name xs) -> (name, xs)
                          (StringVar name xs) -> (name, xs)
       inds <- mapM eval xs -- should we allow expressions?
       is <- checkArrInds inds
       -- add 1, so that range is from 0 to user-specified bound
       let bounds = map (1+) is
       case arr of
                (FloatVar _ _) -> do dimArray name bounds (defVal :: Float)
                                     return ()
                (IntVar _ _) -> do dimArray name bounds (defVal :: Int)
                                   return ()
                (StringVar _ _) -> do dimArray name bounds (defVal :: String)
                                      return ()

interpS _ (LetS var x) =
    do val <- eval x
       setVal var val

interpS _ (PrintS xs nl) =
    do vals <- mapM eval xs
       -- check for type mismatches
       mapM_ printVal vals
       if nl then printString "\n" else return ()

interpS _ (InputS mPrompt vars) =
    do case mPrompt
            of Nothing -> return ()
               (Just ps) -> printString ps
       inputVars vars

interpS jumpTable (GotoS lab) =
    do let maybeCode = lookup lab jumpTable
       assert (isJust maybeCode) ("!BAD GOTO TARGET: " ++ show lab)
       fromJust maybeCode >> end >> return ()

interpS jumpTable (IfS x sts) =
    do val <- eval x
       assert (isFloat val) "!TYPE MISMATCH IN IF"
       if (unFV val)/=0
          then mapM_ (interpS jumpTable) sts
          else return ()

-- Note that the loop condition isn't tested until a NEXT is reached.
-- This is an intentionally authentic feature.  In fact, were we to try to
-- test at the initial FOR, we wouldn't know which NEXT to jump to to skip
-- the loop - it is undecidable.
interpS _ (ForS (FloatVar control []) x1 x2 x3) =
    do v1 <- eval x1
       assert (isFloat v1) "!TYPE MISMATCH IN FOR (INIT)"
       setVar control (unFV v1)
       v2 <- eval x2
       assert (isFloat v2) "!TYPE MISMATCH IN FOR (TO)"
       let lim = unFV v2
       v3 <- eval x3
       assert (isFloat v3) "TYPE MISMATCH IN FOR EXPR (STEP)"
       let step = unFV v3
       trap $ \ x passOn resume continue ->
                  if isNext x || isNextVar control x
                     then do index <- getVar control
                             let index' = index+step
                             setVar control index'
                             if (step>=0 && index'<=lim)
                                    || (step<0 && index>=lim)
                                    then continue True
                                    else resume False
                     else passOn True
interpS _ (ForS _ _ _ _) =
    raiseCC (Fail "!TYPE MISMATCH IN FOR (VAR)") >> return ()

interpS _ (NextS Nothing) = raiseCC (Next Nothing) >> return ()
interpS _ (NextS (Just vars)) = mapM_ interpNextVar vars

interpS jumpTable (GosubS lab) =
    do let maybeCode = lookup lab jumpTable
       assert (isJust maybeCode) ("!BAD GOSUB TARGET: " ++ show lab)
       let f x passOn resume continue =
               if isReturn x then continue False else passOn True
       catchC f (fromJust maybeCode)
       return ()
interpS _ ReturnS = raiseCC Return >> return ()

interpNextVar (FloatVar v []) = raiseCC (Next (Just v)) >> return ()
interpNextVar _ = raiseCC (Fail "!TYPE MISMATCH IN NEXT") >> return ()

inputVars vars =
    do printString "? "
       inText <- getString
       case dataValsP $$ inText
         of [(ivs,"")] ->
                do let vals = zipWith checkInput vars ivs
                   if or (map (==Mismatch) vals)
                     then
                       do printString "!NUMBER EXPECTED - RETRY INPUT LINE\n"
                          printString "?"
                          inputVars vars
                     else
                       do sequence_ (zipWith setVal vars vals)
                          case compare (length vars) (length vals)
                               of LT -> printString "!EXTRA INPUT IGNORED\n"
                                  GT -> do printString "?"
                                           inputVars (drop (length vals) vars)
                                  EQ -> return ()
            _ -> error "Mismatched inputbuf in inputVars"

checkInput (StringVar _ _) s = (StringVal s)
checkInput (IntVar var xs) s = checkInput (FloatVar var xs) s
checkInput (FloatVar _ _) s =
    case readFloat s
         of (Just v) -> (FloatVal v)
	    _ -> Mismatch

getVal :: Var -> Basic (BasicExcep Result ()) Val
getVal (FloatVar name []) =
    do val <- getVar name
       return (FloatVal val)
getVal (IntVar name []) =
    do val <- getVar name
       return (FloatVal (fromIntegral (val :: Int))) -- only vars can be Int
getVal (StringVar name []) =
    do val <- getVar name
       return (StringVal val)
getVal arr =
    do let (name, xs) =
	       case arr of
			(FloatVar name xs) -> (name, xs)
			(IntVar name xs) -> (name, xs)
			(StringVar name xs) -> (name, xs)
       inds <- mapM eval xs
       is <- checkArrInds inds
       case arr of
		(FloatVar _ _) ->
		    do val <- getArr name is
		       return (FloatVal val)
		(IntVar _ _) ->
		    do val <- getArr name is
		       return (FloatVal (fromIntegral (val :: Int)))
		(StringVar _ _) ->
		    do val <- getArr name is
		       return (StringVal val)

setVal :: Var -> Val -> CodeSeg
setVal (FloatVar name []) (FloatVal val) = setVar name val
setVal (FloatVar name []) _ = raiseCC (Fail "!TYPE MISMATCH IN ASSIGNMENT")
setVal (IntVar name []) (FloatVal val) = setVar name (round val :: Int)
setVal (IntVar name []) _ = raiseCC (Fail "!TYPE MISMATCH IN ASSIGNMENT")
setVal (StringVar name []) (StringVal val) = setVar name val
setVal (StringVar name []) _ = raiseCC (Fail "!TYPE MISMATCH IN ASSIGNMENT")
setVal arr val =
    do let (name, xs) =
	       case arr of
			(FloatVar name xs) -> (name, xs)
			(IntVar name xs) -> (name, xs)
			(StringVar name xs) -> (name, xs)
       inds <- mapM eval xs
       is <- checkArrInds inds
       case (arr, val) of
                    ((FloatVar _ _), FloatVal fv) -> setArr name is fv
                    ((IntVar _ _), FloatVal fv) ->
                        setArr name is (round fv :: Int)
                    ((StringVar _ _), StringVal sv) -> setArr name is sv
                    (_,_) -> raiseCC (Fail "!TYPE MISMATCH IN ASSIGNMENT")
       return ()

checkArrInds :: [Val] -> Basic (BasicExcep Result ()) [Int]
checkArrInds inds =
    do assert (and (map isFloat inds)) "!ARRAY DIMS MUST BE NUMBERS"
       assert (and (map (>=0) inds)) ("!NEGATIVE ARRAY DIMS")
       let is = map (round . unFV) inds -- round dimensions as per standard
       return is

-- If Float is a round number, print it as an Int.
printVal :: Val -> Basic o ()
printVal (FloatVal v) =
    let i = floor v :: Integer
        s = if fromInteger i == v then show i else show v
        in printString (" "++s++" ")
printVal (StringVal s) = printString s
printVal Mismatch = printString "<<TYPE MISMATCH>>"
printVal DivByZero = printString "<<DIVISION BY ZERO>>"
