-- BasicInterp.hs
-- The heart of the interpreter, which uses the Basic monad.
-- Lyle Kopnicky

module BasicInterp where

import Data.List
import Data.Maybe
import BasicSyntax
import CPST
import BasicMonad
import ExceptionHandlers
import DurableTraps
import BasicResult
--import Parser      -- for INPUT
--import BasicParser -- for INPUT

data Val = FloatVal Float | StringVal String
	 deriving (Eq,Show,Ord)

boolToVal :: Bool -> Val
boolToVal t = if t then FloatVal (-1) else FloatVal 0

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

liftFVOp1 :: (Float -> Float) -> Val -> Code Val
liftFVOp1 f (FloatVal v1) = return $ FloatVal $ f v1
liftFVOp1 f (StringVal _) = valError "!TYPE MISMATCH IN EXPRESSION"

liftFVOp2 :: (Float -> Float -> Float) -> Val -> Val -> Code Val
liftFVOp2 f (FloatVal v1) (FloatVal v2) = return $ FloatVal $ f v1 v2
liftFVOp2 f _             _             = valError "!TYPE MISMATCH IN EXPRESSION"

liftFVBOp2 :: (Float -> Float -> Bool) -> Val -> Val -> Code Val
liftFVBOp2 f (FloatVal v1) (FloatVal v2) = return $ boolToVal $ f v1 v2
liftFVBOp2 f _             _             = valError "!TYPE MISMATCH IN EXPRESSION"

liftSVOp2 :: (String -> String -> String) -> Val -> Val -> Code Val
liftSVOp2 f (StringVal v1) (StringVal v2) = return $ StringVal $ f v1 v2
liftSVOp2 f _              _              = valError "!TYPE MISMATCH IN EXPRESSION"

-- The return (FloatVal 0) will never be executed, but is needed to make the types work
valError :: String -> Code Val
valError s = basicError s >> return (FloatVal 0)

eval :: Expr -> Code Val
eval (LitX (FloatLit v)) = return (FloatVal v)
eval (LitX (StringLit v)) = return (StringVal v)
eval (VarX var) = getVal var
eval (MinusX x) =
    do val <- eval x
       liftFVOp1 negate val
eval (NotX x) =
    do val <- eval x
       liftFVOp1 (\v -> if v==0 then -1 else 0) val
eval (BinX op x1 x2) =
    do v1 <- eval x1
       v2 <- eval x2
       evalBinOp op v1 v2
eval (ParenX x) = eval x

evalBinOp :: BinOp -> Val -> Val -> Code Val
evalBinOp op =
    case op of
      AddOp -> \v1 v2 -> case (v1,v2) of
                           (FloatVal _,  FloatVal _ ) -> liftFVOp2 (+) v1 v2
                           (StringVal _, StringVal _) -> liftSVOp2 (++) v1 v2
      SubOp -> liftFVOp2 (-)
      MulOp -> liftFVOp2 (*)
      DivOp -> \v1 v2 -> case (v1,v2) of
                           (FloatVal fv1, FloatVal fv2) ->
                               if fv2==0
                                 then valError "!DIVISION BY ZERO"
	                         else return $ FloatVal $ fv1/fv2
                           (_,_) -> valError "!TYPE MISMATCH IN EXPRESSION"
      PowOp -> liftFVOp2 (**)
      EqOp -> liftFVBOp2 (==)
      NEOp -> liftFVBOp2 (/=)
      LTOp -> liftFVBOp2 (<)
      LEOp -> liftFVBOp2 (<=)
      GTOp -> liftFVBOp2 (>)
      GEOp -> liftFVBOp2 (>=)
      AndOp -> liftFVOp2 $ \v1 v2 -> if v1/=0 && v2/=0 then v1 else 0
      OrOp -> liftFVOp2 $ \v1 v2 -> if v1/=0 then v1 else v2
-- TO DO: check defined behavior of AND & OR

-- Interpret a single statement.
-- In the type of interpS, the first () signifies what is passed to a
-- resumed trap.  The second one represents what is returned by interpS.
interpS :: [(Label, Program)] -> Statement -> Code ()

interpS _ (RemS s) = return ()

interpS _ EndS = end

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

{-
interpS _ (InputS mPrompt vars) =
    do case mPrompt
            of Nothing -> return ()
               (Just ps) -> printString ps
       inputVars vars
-}

interpS jumpTable (GotoS lab) =
    do let maybeCode = lookup lab jumpTable
       assert (isJust maybeCode) ("!BAD GOTO TARGET: " ++ show lab)
       fromJust maybeCode >> end

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
       assert (isFloat v3) "!TYPE MISMATCH IN FOR EXPR (STEP)"
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
interpS _ (ForS _ _ _ _) = basicError "!TYPE MISMATCH IN FOR (VAR)"

interpS _ (NextS Nothing) = raiseCC (Next Nothing)
interpS _ (NextS (Just vars)) = mapM_ interpNextVar vars

interpS jumpTable (GosubS lab) =
    do let maybeCode = lookup lab jumpTable
       assert (isJust maybeCode) ("!BAD GOSUB TARGET: " ++ show lab)
       let f x passOn resume continue =
               if isReturn x then continue False else passOn True
       catchC f (fromJust maybeCode)
       return ()
interpS _ ReturnS = raiseCC Return

interpNextVar (FloatVar v []) = raiseCC (Next (Just v))
interpNextVar _ = basicError "!TYPE MISMATCH IN NEXT"

{-
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
-}

getVal :: Var -> Code Val
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

setVal :: Var -> Val -> Code ()
setVal (FloatVar name []) (FloatVal val) = setVar name val
setVal (FloatVar name []) _ = basicError "!TYPE MISMATCH IN ASSIGNMENT"
setVal (IntVar name []) (FloatVal val) = setVar name (round val :: Int)
setVal (IntVar name []) _ = basicError "!TYPE MISMATCH IN ASSIGNMENT"
setVal (StringVar name []) (StringVal val) = setVar name val
setVal (StringVar name []) _ = basicError "!TYPE MISMATCH IN ASSIGNMENT"
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
                    (_,_) -> basicError "!TYPE MISMATCH IN ASSIGNMENT"

checkArrInds :: [Val] -> Basic (BasicExcep BasicResult ()) [Int]
checkArrInds inds =
    do assert (and (map isFloat inds)) "!ARRAY DIMS MUST BE NUMBERS"
       assert (and (map (\(FloatVal ind) -> ind>=0) inds)) ("!NEGATIVE ARRAY DIMS")
       let is = map (round . unFV) inds -- round dimensions as per standard
       return is

-- If Float is a round number, print it as an Int.
printVal :: Val -> Basic o ()
printVal (FloatVal v) =
    let i = floor v :: Integer
        s = if fromInteger i == v then show i else show v
        in printString (" "++s++" ")
printVal (StringVal s) = printString s

