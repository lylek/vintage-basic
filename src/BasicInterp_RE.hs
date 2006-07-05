-- BasicInterp.hs
-- The heart of the interpreter, which uses the Basic monad.
-- Lyle Kopnicky
-- last updated 2004-09-01

module BasicInterp where

import Maybe
import BasicSyntax
import CPST
import BasicMonad
import ExceptionHandlers
import ResumableExceptions
import Parser      -- for INPUT
import BasicParser -- for INPUT

data Val = FloatVal Float | StringVal String | Mismatch | DivBy0
	 deriving (Eq,Show,Ord)

data Result = Pass | Fail String | Next (Maybe String) | Return | Suspend

instance Show Result where
    show Pass = "NORMAL TERMINATION"
    show (Fail s) = s
    show (Next Nothing) = "!NEXT WITHOUT FOR ERROR"
    show (Next (Just s)) = "!NEXT WITHOUT FOR ERROR (VAR "++s++")"
    show Return = "!RETURN WITHOUT GOSUB ERROR"
    show Suspend = "PROGRAM SUSPENDED"
    show _ = "UNDEFINED RESULT"

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

assert cond err = if cond then done else raiseCC (Fail err) >> done

eval :: Expr -> Basic (BasicExcep Result a) Val
eval x = do v <- reset (eval' x)
            case v of
                   Mismatch -> do raiseCC (Fail "!TYPE MISMATCH IN EXPRESSION")
                                  return v
                   DivBy0 -> do raiseCC (Fail "!DIVISON BY ZERO")
                                return v
                   v -> return v

eval' :: Expr -> Basic Val Val
eval' (LitX (FloatLit v)) = return (FloatVal v)
eval' (LitX (StringLit v)) = return (StringVal v)
eval' (VarX (FloatVar var)) =
    do val <- getFloatV var
       return (FloatVal val)
eval' (VarX (StringVar var)) =
    do val <- getStringV var
       return (StringVal val)
eval' (BinX op x1 x2) =
    do v1 <- eval' x1
       v2 <- eval' x2
       case (v1,v2) of
                (FloatVal _, FloatVal _) -> evalBinOp op v1 v2
                (StringVal _, StringVal _) -> evalBinOp op v1 v2
                (_,_) -> raise Mismatch

evalBinOp :: BinOp -> Val -> Val -> Basic Val Val
evalBinOp op v1 v2 =
    case op of
      AddOp -> return (v1+v2)
      SubOp -> if isString v1 then raise Mismatch else return (v1-v2)
      MulOp -> if isString v1 then raise Mismatch else return (v1*v2)
      DivOp -> if isString v1 then raise Mismatch else
                   if v2==0 then raise DivBy0 else return (v1/v2)
      EqOp -> return (boolToVal (v1==v2))
      NEOp -> return (boolToVal (v1/=v2))
      LTOp -> return (boolToVal (v1<v2))
      LTEqOp -> return (boolToVal (v1<=v2))
      GTOp -> return (boolToVal (v1>v2))
      GTEqOp -> return (boolToVal (v1>=v2))

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
    DivBy0 / _ = DivBy0
    _ / DivBy0 = DivBy0
    _ / _ = Mismatch
    recip (FloatVal v1) = FloatVal (recip v1)
    recip DivBy0 = DivBy0
    recip _ = Mismatch
    fromRational v = FloatVal (fromRational v)

bothFV :: Val -> Val -> Bool
bothFV (FloatVal _) (FloatVal _) = True
bothFV _ _ = False

sameV :: Val -> Val -> Bool
sameV (FloatVal _) (FloatVal _) = True
sameV (StringVal _) (StringVal _) = True
sameV _ _ = False

-- Interpret a single statement.
-- In the type of interpS, the first () signifies what is passed to a
-- resumed trap.  The second one represents what is returned by interpS.
interpS :: Statement -> Basic (BasicExcep Result ()) ()

interpS (RemS s) = return ()

interpS EndS = end >> return ()

interpS (LetS v x) =
    do val <- eval x
       case (v,val) of
                (FloatVar var, FloatVal val) -> setFloatV var val
                (StringVar var, StringVal val) -> setStringV var val
                (_,_) -> do raiseCC (Fail "!TYPE MISMATCH IN LET")
                            return ()

interpS (PrintS xs nl) =
    do vals <- sequence (map eval xs)
       -- check for type mismatches
       sequence (map printVal vals)
       if nl then printString "\n" else return ()

interpS (InputS mPrompt vars) =
    do case mPrompt
            of Nothing -> return ()
               (Just ps) -> printString ps
       printString "? "
       inText <- getString
       case inputBufP `ap` inText
            of [(inputBuf,"")] -> inputVars vars inputBuf
               _ -> error "Mismatched inputbuf"


interpS (GotoS lab) =
    do maybeBCont <- lookupLabel lab
       assert (isJust maybeBCont) ("!BAD GOTO TARGET: "++show lab)
       interp (fromJust maybeBCont) >> end >> return ()

interpS (IfS x sts) =
    do val <- eval x
       assert (isFloat val) "!TYPE MISMATCH IN IF"
       if (unFV val)/=0 then interp sts >> return () else return ()

-- Note that the loop condition isn't tested the first time through.
-- This is an intentionally authentic feature.
interpS (ForS (StringVar _) x1 x2 x3) =
    raiseCC (Fail "!TYPE MISMATCH IN FOR (VAR)") >> return ()
interpS (ForS (FloatVar var) x1 x2 x3) =
    do v1 <- eval x1
       assert (isFloat v1) "!TYPE MISMATCH IN FOR (INIT)"
       setFloatV var (unFV v1)
       v2 <- eval x2
       assert (isFloat v2) "!TYPE MISMATCH IN FOR (TO)"
       let lim = unFV v2
       v3 <- eval x3
       assert (isFloat v3) "TYPE MISMATCH IN FOR EXPR (STEP)"
       let step = unFV v3
       callCC (\loop -> trap isMyVar (updtest loop lim step))
    where isMyVar x = isNext x || isNextVar var x
          updtest loop lim step x loopDone handler =
              do index <- getFloatV var
                 let index' = index+step
                 setFloatV var index'
                 if (step>=0 && index'<=lim) || (step<0 && index>=lim)
                    then install handler >> loop ()
                    else loopDone ()


interpS (NextS Nothing) = raiseCC (Next Nothing) >> return ()
interpS (NextS (Just vars)) = sequence_ (map interpNextVar vars)

interpS (GosubS lab) =
    do maybeStatements <- lookupLabel lab
       assert (isJust maybeStatements) ("!BAD GOSUB TARGET: " ++ show lab)
       callCC $ \k -> do trap isReturn (\_ _ _ -> k ())
                         interp (fromJust maybeStatements)
                         end
                         return ()
interpS ReturnS = raiseCC Return >> return ()

interpNextVar (FloatVar v) = raiseCC (Next (Just v)) >> return ()
interpNextVar _ = raiseCC (Fail "!TYPE MISMATCH IN NEXT") >> return ()

inputVars [] [] = return ()
inputVars [] _ = printString "!EXTRA INPUT IGNORED\n"
inputVars vars [] =
    do printString "?? "
       inText <- getString
       case inputBufP `ap` inText
            of [(inputBuf,"")] -> inputVars vars inputBuf
               _ -> error "Mismatched inputbuf"
inputVars ((StringVar var):vars) (iv:ivs) =
    do setStringV var iv
       inputVars vars ivs
inputVars ((FloatVar var):vars) (iv:ivs) =
    do case forceFloatP `ap` iv
            of [(fv,"")] -> do setFloatV var fv
                               inputVars vars ivs
               _ -> error "Error in forceFloatP"

-- Interpret a sequence of statements.
-- The () represents what is fed to a resumed trap.
interp :: [Statement] -> Basic (BasicExcep Result ()) (BasicExcep Result ())
interp sts = sequence_ (map interpS sts) >> done

-- If Float is a round number, print it as an Int.
printVal :: Val -> Basic o ()
printVal (FloatVal v) =
    let i = floor v :: Integer
        s = if fromInteger i == v then show i else show v
        in printString (" "++s++" ")
printVal (StringVal s) = printString s
printVal Mismatch = printString "<<TYPE MISMATCH>>"
printVal DivBy0 = printString "<<DIVISION BY ZERO>>"
