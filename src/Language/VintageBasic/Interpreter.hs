{-# LANGUAGE FlexibleContexts, ParallelListComp, Rank2Types #-}

-- | The heart of the BASIC interpreter. It is implemented on top
-- of the BASIC monad.

module Language.VintageBasic.Interpreter(interpLines) where

import Control.Monad.CPST.DurableTraps
import Data.List
import Data.Maybe
import Language.VintageBasic.Builtins(Builtin(..))
import Language.VintageBasic.LexCommon(Tagged(..))
import Language.VintageBasic.BasicMonad
import Language.VintageBasic.Result
import Language.VintageBasic.Printer(printFloat)
import Language.VintageBasic.RuntimeParser(dataValsP,readFloat,trim)
import Language.VintageBasic.Syntax
import Text.ParserCombinators.Parsec(parse)
import Text.ParserCombinators.Parsec.Pos(sourceLine)

-- | Entry in the jump table, a lookup table that holds both
-- program code and @DATA@ statements, indexed by 'Label'.
data JumpTableEntry = JumpTableEntry {
    jtLabel :: Label,     -- ^ the starting line number
    jtProgram :: Program, -- ^ program source starting at that line
    jtData :: [String]    -- ^ @DATA@ strings starting at that line
}

type JumpTable = [JumpTableEntry]

programLookup :: JumpTable -> Label -> (Maybe Program)
programLookup jt lab = lookup lab [(jtLabel jte, jtProgram jte) | jte <- jt]

dataLookup :: JumpTable -> Label -> (Maybe [String])
dataLookup jt lab = lookup lab [(jtLabel jte, jtData jte) | jte <- jt]

-- | Lazily interprets a list of BASIC lines of code.

interpLines :: [Line] -> Program

-- Note that jumpTable and interpLine are mutually recursive.
-- The jumpTable contains interpreted code, which in turn calls
-- the jumpTable to look up code.  Since the jumpTable is a single
-- data structure, it memoizes interpreted code, making 'interpLines'
-- a just-in-time compiler.  (The only time code is reinterpreted
-- is following an @IF@ statement.)

interpLines progLines =
    let interpLine line@(Line lab stmts) =
            (lab, mapM_ (interpTS jumpTable) stmts, dataFromLine line)
        makeTableEntry (accumCode, accumData) (lab, codeSeg, lineData) =
            let accumCode' = codeSeg >> accumCode
                accumData' = lineData ++ accumData
                in ((accumCode', accumData'), JumpTableEntry lab accumCode' accumData')
        jumpTable = snd $ mapAccumR makeTableEntry (done, []) $ map interpLine progLines
    in do
        case jumpTable of
            ((JumpTableEntry _ prog dat) : _) -> do
                seedRandomFromTime
                setDataStrings dat
                prog
            [] -> done

-- Expression evaluation

boolToVal :: Bool -> Val
boolToVal t = if t then FloatVal (-1) else FloatVal 0

isNext :: Result -> Bool
isNext (LabeledRuntimeException _ (Next Nothing)) = True
isNext _ = False

isNextVar :: VarName -> Result -> Bool
isNextVar (VarName FloatType v1) (LabeledRuntimeException _ (Next (Just v2))) = v1==v2
isNextVar _ _ = False

isReturn :: Result -> Bool
isReturn (LabeledRuntimeException _ Return) = True
isReturn _ = False

liftFVOp1 :: (Float -> Float) -> Val -> Code Val
liftFVOp1 f (FloatVal v1) =
    let v = f v1 in seq v $ return $ FloatVal v
liftFVOp1 _ _             = typeMismatch

liftFVBuiltin1 :: (Float -> Float) -> [Val] -> Code Val
liftFVBuiltin1 f [FloatVal v1] =
    let v = f v1 in seq v $ return $ FloatVal v
liftFVBuiltin1 _ [_] = typeMismatch
liftFVBuiltin1 _ _ = wrongNumArgs

liftFVOp2 :: (Float -> Float -> Float) -> Val -> Val -> Code Val
liftFVOp2 f (FloatVal v1) (FloatVal v2) =
    let v = f v1 v2 in seq v $ return $ FloatVal v
liftFVOp2 _ _             _             = typeMismatch

liftSVOp2 :: (String -> String -> String) -> Val -> Val -> Code Val
liftSVOp2 f (StringVal v1) (StringVal v2) =
    let v = f v1 v2 in seq v $ return $ StringVal v
liftSVOp2 _ _              _              = typeMismatch

liftFSCmpOp2 :: (forall a. Ord a => a -> a -> Bool) -> Val -> Val -> Code Val
liftFSCmpOp2 f v1 v2 = do
    assert (typeOf v1 == typeOf v2) TypeMismatchError
    let v = f v1 v2
    seq v $ return $ boolToVal v

valError :: RuntimeError -> Code Val
-- The return (FloatVal 0) will never be executed, but is needed to make the types work out.
valError err = raiseRuntimeError err >> return (FloatVal 0)

wrongNumArgs, typeMismatch, invalidArgument, divisionByZero :: Code Val
wrongNumArgs    = valError WrongNumberOfArgumentsError
typeMismatch    = valError TypeMismatchError
invalidArgument = valError InvalidArgumentError
divisionByZero  = valError DivisionByZeroError

-- | Evaluate a BASIC expression.
eval :: Expr -> Code Val
eval (LitX (FloatLit v)) = return (FloatVal v)
eval (LitX (StringLit v)) = return (StringVal v)
eval (VarX var) = getVar var
eval (FnX var xs) = do
    fn <- getFn var
    vals <- mapM eval xs
    fn vals
eval (MinusX x) = do
    val <- eval x
    liftFVOp1 negate val
eval (NotX x) = do
    val <- eval x
    liftFVOp1 (\v -> if v==0 then -1 else 0) val
eval (BinX op x1 x2) = do
    v1 <- eval x1
    v2 <- eval x2
    evalBinOp op v1 v2
eval (BuiltinX b xs) = do
    vs <- mapM eval xs
    evalBuiltin b vs
eval NextZoneX = do
    curCol <- getOutputColumn
    let numSpaces = zoneWidth - (curCol `mod` zoneWidth)
    return $ StringVal $ replicate numSpaces ' '
eval EmptySeparatorX = return $ StringVal ""
eval (ParenX x) = eval x

-- | Evaluate an expression with a binary operator.
evalBinOp :: BinOp -> Val -> Val -> Code Val
evalBinOp op =
    case op of
        AddOp -> \v1 v2 ->
            case (v1,v2) of
                (FloatVal _,  FloatVal _ ) -> liftFVOp2 (+) v1 v2
                (StringVal _, StringVal _) -> liftSVOp2 (++) v1 v2
                (_,           _          ) -> typeMismatch
        SubOp -> liftFVOp2 (-)
        MulOp -> liftFVOp2 (*)
        DivOp -> \v1 v2 ->
            case (v1,v2) of
                (FloatVal fv1, FloatVal fv2) ->
                    if fv2==0
                        then divisionByZero
                        else let v = fv1/fv2 in seq v $ return $ FloatVal v
                (_,_) -> typeMismatch
        PowOp -> liftFVOp2 (**)
        EqOp -> liftFSCmpOp2 (==)
        NEOp -> liftFSCmpOp2 (/=)
        LTOp -> liftFSCmpOp2 (<)
        LEOp -> liftFSCmpOp2 (<=)
        GTOp -> liftFSCmpOp2 (>)
        GEOp -> liftFSCmpOp2 (>=)
        AndOp -> liftFVOp2 $ \v1 v2 -> if v1/=0 && v2/=0 then v1 else 0
        OrOp -> liftFVOp2 $ \v1 v2 -> if v1/=0 then v1 else v2

checkArgTypes :: [ValType] -> [Val] -> Code ()
checkArgTypes types vals = do
    if length types == length vals
        then
            if and [t == typeOf v | t <- types | v <- vals]
                then return ()
                else typeMismatch >> return ()
        else
            wrongNumArgs >> return ()

-- | Evaluate an expression with a builtin function.
evalBuiltin :: Builtin -> [Val] -> Code Val
evalBuiltin builtin args = case builtin of
    AbsBI -> liftFVBuiltin1 abs args
    AscBI -> do
        checkArgTypes [StringType] args
        let [StringVal v] = args
        if length v == 0
            then invalidArgument
            else return $ FloatVal (fromIntegral (fromEnum (head v)))
    AtnBI -> liftFVBuiltin1 atan args
    ChrBI -> do
        checkArgTypes [FloatType] args
        let [FloatVal v] = args
        let iv = floatToInt v
        if iv < 0 || iv > 255
            then invalidArgument
            else return $ StringVal [toEnum iv]
    CosBI -> liftFVBuiltin1 cos args
    ExpBI -> liftFVBuiltin1 exp args
    IntBI -> liftFVBuiltin1 (fromIntegral . floatToInt) args
    LeftBI -> do
        checkArgTypes [StringType, FloatType] args
        let [StringVal sv, FloatVal fv] = args
        let iv = floatToInt fv
        if iv < 0
            then invalidArgument
            else return (StringVal (take iv sv))
    LenBI -> do
        checkArgTypes [StringType] args
        let [StringVal sv] = args in return (FloatVal (fromIntegral (length sv)))
    LogBI -> do
        checkArgTypes [FloatType] args
        let [FloatVal fv] = args in if fv <= 0 then invalidArgument else return (FloatVal (log fv))
    MidBI -> case args of
        [StringVal sv, FloatVal fv] ->
            let iv = floatToInt fv in
                if iv < 1
                    then invalidArgument
                    else return (StringVal (drop (iv-1) sv))
        [_, _] -> typeMismatch
        [StringVal sv, FloatVal fv1, FloatVal fv2] ->
            let iv1 = floatToInt fv1
                iv2 = floatToInt fv2
            in
                if iv1 < 1 || iv2 < 0
                    then invalidArgument
                    else return (StringVal (take iv2 (drop (iv1-1) sv)))
        [_, _, _] -> typeMismatch
        _ -> wrongNumArgs
    RightBI -> do
        checkArgTypes [StringType, FloatType] args
        let [StringVal sv, FloatVal fv] = args
        let iv = floatToInt fv
        if iv < 0
            then invalidArgument
            else return (StringVal (drop (length sv - iv) sv))
    RndBI -> do
        checkArgTypes [FloatType] args
        let [FloatVal fv] = args
        let iv = floatToInt fv
        if iv < 0
            then seedRandom iv
            else return ()
        rv <- if (iv == 0) then getPrevRandom else getRandom
        return (FloatVal rv)
    SgnBI -> liftFVBuiltin1 (\v -> if v < 0 then -1 else if v > 0 then 1 else 0) args
    SinBI -> liftFVBuiltin1 sin args
    SpcBI -> do
        checkArgTypes [FloatType] args
        let [FloatVal fv] = args
        let iv = floatToInt fv
        if iv < 0
           then invalidArgument
           else return (StringVal (replicate iv ' '))
    SqrBI -> do
        checkArgTypes [FloatType] args
        let [FloatVal fv] = args
        if fv < 0
            then invalidArgument
            else return (FloatVal (sqrt fv))
    StrBI -> do
        checkArgTypes [FloatType] args
        let [FloatVal fv] = args in return (StringVal (showVal (FloatVal fv)))
    TabBI -> do
        checkArgTypes [FloatType] args
        let [FloatVal fv] = args
        let destCol = floatToInt fv
        if (destCol < 0)
          then invalidArgument
          else do
            curCol <- getOutputColumn
            return $ StringVal $
                if curCol > destCol
                  then ""
                  else replicate (destCol - curCol) ' '
    TanBI -> liftFVBuiltin1 tan args
    ValBI -> do
        checkArgTypes [StringType] args
        let [StringVal sv] = args in return (FloatVal (maybe 0 id (readFloat (trim sv))))

-- | Interpret a tagged statement.
-- Sets the line number in the state, then passes the statement on to interpS.
interpTS :: JumpTable -> Tagged Statement -> Code ()
interpTS jumpTable (Tagged pos statement) = do
    setLineNumber (sourceLine pos)
    interpS jumpTable statement

-- | Interpret a single statement.
-- In the type of interpS, the first () signifies what is passed to a
-- resumed trap.  The second one represents what is returned by interpS.
interpS :: JumpTable -> Statement -> Code ()

interpS _ (RemS _) = return ()

interpS _ EndS = end

interpS _ StopS = end

interpS _ (DimS arrs) = mapM_ interpDim arrs

interpS _ (LetS var x) = do
    val <- eval x
    setVar var val

interpS _ (PrintS xs) = do
    _ <- mapM (\x -> eval x >>= printVal) xs
    if null xs || not (isPrintSeparator (last xs))
        then printString "\n"
        else return ()

interpS _ (InputS mPrompt vars) = do
    case mPrompt of
        Nothing -> return ()
        (Just ps) -> printString ps
    inputVars vars

interpS jumpTable (GotoS lab) = do
    let maybeCode = programLookup jumpTable lab
    assert (isJust maybeCode) (BadGotoTargetError lab)
    fromJust maybeCode >> end

interpS jumpTable (OnGotoS x labs) = interpComputed jumpTable GotoS x labs

interpS jumpTable (OnGosubS x labs) = interpComputed jumpTable GosubS x labs

interpS jumpTable (IfS x sts) = do
    v <- eval x
    fv <- extractFloatOrFail TypeMismatchError v
    if fv == 0
        then return ()
        else mapM_ (interpTS jumpTable) sts

-- Note that the loop condition isn't tested until a NEXT is reached.
-- This is an intentionally authentic feature.  In fact, were we to try to
-- test at the initial FOR, we wouldn't know which NEXT to jump to to skip
-- the loop - it is undecidable.
interpS _ (ForS control x1 x2 x3) = do
    assert (typeOf control == FloatType) TypeMismatchError
    v1 <- eval x1
    _ <- extractFloatOrFail TypeMismatchError v1
    setScalarVar control v1
    v2 <- eval x2
    lim <- extractFloatOrFail TypeMismatchError v2
    v3 <- eval x3
    step <- extractFloatOrFail TypeMismatchError v3
    trap $ \ x passOn resume continue ->
        if isNext x || isNextVar control x
            then do
                indexVal <- getScalarVar control
                let index = case indexVal of
                        FloatVal i -> i
                        _ -> error "expected float val for FOR index"
                    index' = index+step
                setScalarVar control (FloatVal index')
                if (step>=0 && index'<=lim)
                    || (step<0 && index'>=lim)
                    then continue True
                    else resume False
            else passOn True

interpS _ (NextS Nothing) = raiseRuntimeException (Next Nothing)
interpS _ (NextS (Just vars)) = mapM_ interpNextVar vars

interpS jumpTable (GosubS lab) =
    do let maybeCode = programLookup jumpTable lab
       assert (isJust maybeCode) (BadGosubTargetError lab)
       _ <- catchC gosubHandler (fromJust maybeCode)
       return ()
interpS _ ReturnS = raiseRuntimeException Return

interpS _ RandomizeS = seedRandomFromTime

interpS jumpTable (RestoreS maybeLab) = do
   case maybeLab of
       (Just lab) -> case dataLookup jumpTable lab of
           (Just ds) -> setDataStrings ds
           Nothing -> raiseRuntimeError (BadRestoreTargetError lab)
       Nothing -> if null jumpTable
           then return ()
           else setDataStrings (jtData (head jumpTable))

interpS _ (ReadS vars) = mapM_ interpRead vars

interpS _ (DataS _) = return ()

interpS _ (DefFnS vn params expr) = setFn vn $ \vals -> do
    assert (length params == length vals) WrongNumberOfArgumentsError
    assert
        (and [typeOf p == typeOf v | p <- params | v <- vals])
        TypeMismatchError
    stashedVals <- mapM getScalarVar params
    sequence_ $ zipWith setScalarVar params vals
    result <- eval expr
    sequence_ $ zipWith setScalarVar params stashedVals
    return result

gosubHandler :: BasicExceptionHandler
gosubHandler x passOn _ continue = if isReturn x then continue False else passOn True

-- | Interpret a computed @GOTO@ or @GOSUB@.
interpComputed :: JumpTable -> (Label -> Statement) -> Expr -> [Label] -> Code ()
interpComputed jumpTable cons x labs = do
    v <- eval x
    fv <- extractFloatOrFail TypeMismatchError v
    let i = floatToInt fv
    if i > 0 && i <= length labs
        then do
            let lab = labs !! (i-1)
            interpS jumpTable (cons lab)
        else
            return ()

-- | Interpret a @NEXT@ with a supplied variable.
interpNextVar :: VarName -> Code ()
interpNextVar (VarName FloatType v) = raiseRuntimeException (Next (Just v))
interpNextVar _ = raiseRuntimeError TypeMismatchError

-- | Execute an @INPUT@ statement with a list of input variables.
inputVars :: [Var] -> Code ()
inputVars vars = do
    printString "? "
    inText <- getString
    case parse dataValsP "" inText of
        (Right ivs) -> do
            let maybeVals = zipWith checkInput vars ivs
            if or (map isNothing maybeVals)
                then do
                    printString "!NUMBER EXPECTED - RETRY INPUT LINE\n"
                    inputVars vars
                else do
                    let vals = map fromJust maybeVals
                    sequence_ (zipWith setVar vars vals)
                    case compare (length vars) (length ivs) of
                        LT -> printString "!EXTRA INPUT IGNORED\n"
                        GT -> do
                            printString "?"
                            inputVars (drop (length vals) vars)
                        EQ -> return ()
        (Left _) -> error "Mismatched inputbuf in inputVars"

checkInput :: Var -> String -> Maybe Val
checkInput var s = case typeOf var of
    StringType -> Just (StringVal s)
    FloatType  -> case readFloat s of
        (Just v) -> Just (FloatVal v)
        _ -> Nothing
    IntType -> case readFloat s of
        (Just v) -> Just (IntVal (floatToInt v))
        _ -> Nothing

interpRead :: Var -> Code ()
interpRead var = do
    s <- readData
    case checkInput var s of
        Nothing -> raiseRuntimeError TypeMismatchError
        (Just val) -> setVar var val

getVar :: Var -> Code Val
getVar (ScalarVar vn) = do
    val <- getScalarVar vn
    coerceToExpression val
getVar (ArrVar vn xs) = do
    inds <- mapM eval xs
    is <- checkArrInds inds
    val <- getArrVar vn is
    coerceToExpression val

setVar :: Var -> Val -> Code ()
setVar (ScalarVar vn) val = do
    val' <- coerce vn val
    setScalarVar vn val'
setVar (ArrVar vn xs) val = do
    inds <- mapM eval xs
    is <- checkArrInds inds
    val' <- coerce vn val
    setArrVar vn is val'

-- | Coerce a value to the type it would have in an expression.
-- Specifically coerces ints to floats.
coerceToExpression :: Val -> Code Val
coerceToExpression val = case typeOf val of
    IntType -> coerce FloatType val
    _       -> return val

coerce :: Typeable a => a -> Val -> Code Val
coerce var val = case (typeOf var, val) of
    (IntType,    (FloatVal fv)) -> return $ IntVal (floatToInt fv)
    (FloatType,  (FloatVal _))  -> return val
    (FloatType,  (IntVal iv))   -> return $ FloatVal (fromIntegral iv)
    (IntType,    (IntVal _))    -> return val
    (StringType, (StringVal _)) -> return val
    (_,          _)             -> typeMismatch

interpDim :: (VarName, [Expr]) -> Code ()
interpDim (vn, xs) = do
    inds <- mapM eval xs
    is <- checkArrInds inds
    -- add 1, so that range is from 0 to user-specified bound
    let bounds = map (1+) is
    _ <- dimArray vn bounds
    return ()

checkArrInds :: [Val] -> Basic (BasicExcep Result ()) [Int]
checkArrInds indVals = do
    indFs <- mapM (extractFloatOrFail TypeMismatchError) indVals
    assert (and (map (>=0) indFs)) NegativeArrayDimError
    let inds = map floatToInt indFs
    return inds

showVal :: Val -> String
showVal (FloatVal fv) = printFloat fv
showVal (IntVal iv) = if iv > 0 then " " else "" ++ show iv
showVal (StringVal s) = s

printVal :: Val -> Basic o ()
printVal v =
  printString (showVal v ++ case v of
      (FloatVal _)  -> " "
      (IntVal _)    -> " "
      (StringVal _) -> ""
  )

-- | Extract the @DATA@ strings from a program line.
dataFromLine :: Line -> [String]
dataFromLine (Line _ stmts) = concat (map (dataFromStatement . getTaggedVal) stmts)

dataFromStatement :: Statement -> [String]
dataFromStatement (DataS s) =
    let (Right ss) = parse dataValsP "" s
    in ss
dataFromStatement _ = []
