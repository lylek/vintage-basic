-- BasicInterp.hs
-- The heart of the interpreter, which uses the Basic monad.
-- Lyle Kopnicky
-- last updated 2005-07-06

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
eval' (VarX (FloatVar var)) =
    do val <- getVal var
       return (FloatVal val)
eval' (VarX (IntVar var)) =
    do val <- getVal var
       return (FloatVal (fromIntegral (val :: Int))) -- only vars can be Int
eval' (VarX (StringVar var)) =
    do val <- getVal var
       return (StringVal val)
eval' (ArrX (FloatArr var xs)) =
    do inds <- mapM eval xs
       is <- checkArrInds inds
       val <- getArrVal var is
       return (FloatVal val)
eval' (ArrX (IntArr var xs)) =
    do inds <- mapM eval xs
       is <- checkArrInds inds
       val <- getArrVal var is
       return (FloatVal (fromIntegral (val :: Int)))
eval' (ArrX (StringArr var xs)) =
    do inds <- mapM eval xs
       is <- checkArrInds inds
       val <- getArrVal var is
       return (StringVal val)
eval' (MinusX x) =
    do val <- eval x
       return (-val)
eval' (NotX x) =
    do val <- eval x
       return (if val==0 then -1 else 0)
eval' (BinX op x1 x2) =
    do v1 <- eval' x1
       v2 <- eval' x2
       case (v1,v2) of
                (FloatVal _, FloatVal _) -> evalBinOp op v1 v2
                (StringVal _, StringVal _) -> evalBinOp op v1 v2
                (_,_) -> return Mismatch
eval' (ParenX x) = eval x

evalBinOp :: BinOp -> Val -> Val -> Basic (BasicExcep Result ()) Val
evalBinOp op v1 v2 =
    case op of
      AddOp -> return (v1+v2)
      SubOp -> if isString v1 then return Mismatch
               else return (v1-v2)
      MulOp -> if isString v1 then return Mismatch
               else return (v1*v2)
      DivOp -> if isString v1 then return Mismatch
               else if v2==0 then return DivByZero
                    else return (v1/v2)
      PowOp -> if isString v1 then return Mismatch
               else return (v1**v2)
      EqOp -> return (boolToVal (v1==v2))
      NEOp -> return (boolToVal (v1/=v2))
      LTOp -> return (boolToVal (v1<v2))
      LTEqOp -> return (boolToVal (v1<=v2))
      GTOp -> return (boolToVal (v1>v2))
      GTEqOp -> return (boolToVal (v1>=v2))
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
    sqrt (FloatVal v) = FloatVal (sqrt v)
    log (FloatVal v) = FloatVal (log v)
    (FloatVal v1) ** (FloatVal v2) = FloatVal (v1**v2)
    logBase (FloatVal v1) (FloatVal v2) = FloatVal (logBase v1 v2)
    sin (FloatVal v) = FloatVal (sin v)
    tan (FloatVal v) = FloatVal (tan v)
    cos (FloatVal v) = FloatVal (cos v)
    asin (FloatVal v) = FloatVal (asin v)
    atan (FloatVal v) = FloatVal (atan v)
    acos (FloatVal v) = FloatVal (acos v)
    sinh (FloatVal v) = FloatVal (sinh v)
    tanh (FloatVal v) = FloatVal (tanh v)
    cosh (FloatVal v) = FloatVal (cosh v)
    asinh (FloatVal v) = FloatVal (asinh v)
    atanh (FloatVal v) = FloatVal (atanh v)
    acosh (FloatVal v) = FloatVal (acosh v)

bothFV :: Val -> Val -> Bool
bothFV (FloatVal _) (FloatVal _) = True
bothFV _ _ = False

sameV :: Val -> Val -> Bool
sameV (FloatVal _) (FloatVal _) = True
sameV (StringVal _) (StringVal _) = True
sameV _ _ = False

checkArrInds :: [Val] -> Basic (BasicExcep Result ()) [Int]
checkArrInds inds =
    do assert (and (map isFloat inds)) "!ARRAY DIMS MUST BE NUMBERS"
       assert (and (map (>=0) inds)) ("!NEGATIVE ARRAY DIMS")
       let is = map (round . unFV) inds -- round dimensions as per standard
       return is

-- Interpret a single statement.
-- In the type of interpS, the first () signifies what is passed to a
-- resumed trap.  The second one represents what is returned by interpS.
interpS :: [(Label, Code)] -> Statement -> CodeSeg

interpS _ (RemS s) = return ()

interpS _ EndS = end >> return ()

interpS _ (DimS arr) =
    do let (v, xs) = case arr of
                          (FloatArr v xs) -> (v, xs)
                          (IntArr v xs) -> (v, xs)
                          (StringArr v xs) -> (v, xs)
       inds <- mapM eval xs -- should we allow expressions?
       is <- checkArrInds inds
       -- add 1, so that range is from 0 to user-specified bound
       let bounds = map (1+) is
       case arr of
                (FloatArr _ _) -> do dimArray v bounds (defVal :: Float)
                                     return ()
                (IntArr _ _) -> do dimArray v bounds (defVal :: Int)
                                   return ()
                (StringArr _ _) -> do dimArray v bounds (defVal :: String)
                                      return ()

interpS _ (LetS v x) =
    do val <- eval x
       case (v,val) of
                (FloatVar var, FloatVal val) -> setVal var val
                (IntVar var, FloatVal val) -> setVal var (round val :: Int)
                -- should use floor instead?
                (StringVar var, StringVal val) -> setVal var val
                (_,_) -> do raiseCC (Fail "!TYPE MISMATCH IN LET")
                            return ()

interpS _ (LetArrS arr expr) =
    do let (v, xs) = case arr of
                              (FloatArr v xs) -> (v, xs)
                              (IntArr v xs) -> (v, xs)
                              (StringArr v xs) -> (v, xs)
       inds <- mapM eval xs
       is <- checkArrInds inds
       val <- eval expr
       case (arr,val) of
                    ((FloatArr _ _), FloatVal fv) -> setArrVal v is fv
                    ((IntArr _ _), FloatVal fv) ->
                        setArrVal v is (round fv :: Int)
                    ((StringArr _ _), StringVal sv) -> setArrVal v is sv
                    (_,_) -> raiseCC (Fail "!TYPE MISMATCH IN ARRAY LET")
       return ()

interpS _ (PrintS xs nl) =
    do vals <- mapM eval xs
       -- check for type mismatches
       mapM_ printVal vals
       if nl then printString "\n" else return ()

interpS _ (InputS mPrompt vars) =
    do case mPrompt
            of Nothing -> return ()
               (Just ps) -> printString ps
       printString "? "
       inText <- getString
       case inputBufP $$ inText
            of [(inputBuf,"")] -> inputVars vars inputBuf
               _ -> error "Mismatched inputbuf"

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

-- Note that the loop condition isn't tested the first time through.
-- This is an intentionally authentic feature.
interpS _ (ForS (StringVar _) x1 x2 x3) =
    raiseCC (Fail "!TYPE MISMATCH IN FOR (VAR)") >> return ()
interpS _ (ForS (FloatVar var) x1 x2 x3) =
    do v1 <- eval x1
       assert (isFloat v1) "!TYPE MISMATCH IN FOR (INIT)"
       setVal var (unFV v1)
       v2 <- eval x2
       assert (isFloat v2) "!TYPE MISMATCH IN FOR (TO)"
       let lim = unFV v2
       v3 <- eval x3
       assert (isFloat v3) "TYPE MISMATCH IN FOR EXPR (STEP)"
       let step = unFV v3
       trap $ \ x passOn resume continue ->
                  if isNext x || isNextVar var x
                     then do index <- getVal var
                             let index' = index+step
                             setVal var index'
                             if (step>=0 && index'<=lim)
                                    || (step<0 && index>=lim)
                                    then continue True
                                    else resume False
                     else passOn True

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

interpNextVar (FloatVar v) = raiseCC (Next (Just v)) >> return ()
interpNextVar _ = raiseCC (Fail "!TYPE MISMATCH IN NEXT") >> return ()

inputVars [] [] = return ()
inputVars [] _ = printString "!EXTRA INPUT IGNORED\n"
inputVars vars [] =
    do printString "?? "
       inText <- getString
       case inputBufP $$ inText
            of [(inputBuf,"")] -> inputVars vars inputBuf
               _ -> error "Mismatched inputbuf"
inputVars ((StringVar var):vars') (iv:ivs) =
    do setVal var iv
       inputVars vars' ivs
inputVars vars@((FloatVar var):vars') (iv:ivs) =
    do case floatP $$ iv
            of [(fv,"")] -> do setVal var fv
                               inputVars vars' ivs
               _ -> do printString "!NUMBER EXPECTED - RETRY\n"
                       inputVars vars []

-- If Float is a round number, print it as an Int.
printVal :: Val -> Basic o ()
printVal (FloatVal v) =
    let i = floor v :: Integer
        s = if fromInteger i == v then show i else show v
        in printString (" "++s++" ")
printVal (StringVal s) = printString s
printVal Mismatch = printString "<<TYPE MISMATCH>>"
printVal DivByZero = printString "<<DIVISION BY ZERO>>"
