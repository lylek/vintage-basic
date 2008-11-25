{-# LANGUAGE FlexibleContexts, ParallelListComp, Rank2Types #-}

-- BasicInterp.hs
-- The heart of the interpreter, which uses the Basic monad.
-- Lyle Kopnicky

module BasicInterp(interpLines) where

import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec(parse)
import Text.ParserCombinators.Parsec.Pos(sourceLine)
import DurableTraps
import BasicBuiltin(Builtin(..))
import BasicLexCommon(Tagged(..))
import BasicMonad
import BasicResult
import BasicRuntimeParser(dataValsP,readFloat)
import BasicSyntax

data JumpTableEntry = JumpTableEntry {
    jtLabel :: Label,
    jtProgram :: Program,
    jtData :: [String]
}

type JumpTable = [JumpTableEntry]

programLookup :: JumpTable -> Label -> (Maybe Program)
programLookup jt lab = lookup lab [(jtLabel jte, jtProgram jte) | jte <- jt]

dataLookup :: JumpTable -> Label -> (Maybe [String])
dataLookup jt lab = lookup lab [(jtLabel jte, jtData jte) | jte <- jt]

-- Note that jumpTable and interpLine are mutually recursive.
-- The jumpTable contains interpreted code, which in turn calls
-- the jumpTable to look up code.  Since the jumpTable is a single
-- data structure, it memoizes interpreted code, making 'program'
-- a just-in-time compiler.  (The only time code is reinterpreted
-- is following an IF statement.)
interpLines :: [Line] -> Program
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

isNext :: BasicResult -> Bool
isNext (LabeledRuntimeException _ (Next Nothing)) = True
isNext _ = False

isNextVar :: VarName -> BasicResult -> Bool
isNextVar (VarName FloatType v1) (LabeledRuntimeException _ (Next (Just v2))) = v1==v2
isNextVar _ _ = False

isReturn :: BasicResult -> Bool
isReturn (LabeledRuntimeException _ Return) = True
isReturn _ = False

liftFVOp1 :: (Float -> Float) -> Val -> Code Val
liftFVOp1 f (FloatVal v1) = return $ FloatVal $ f v1
liftFVOp1 _ _             = typeMismatch

liftFVBuiltin1 :: (Float -> Float) -> [Val] -> Code Val
liftFVBuiltin1 f [FloatVal v1] = return $ FloatVal $ f v1
liftFVBuiltin1 _ _ = typeMismatch

liftFVOp2 :: (Float -> Float -> Float) -> Val -> Val -> Code Val
liftFVOp2 f (FloatVal v1) (FloatVal v2) = return $ FloatVal $ f v1 v2
liftFVOp2 _ _             _             = typeMismatch

liftFVBOp2 :: (Float -> Float -> Bool) -> Val -> Val -> Code Val
liftFVBOp2 f (FloatVal v1) (FloatVal v2) = return $ boolToVal $ f v1 v2
liftFVBOp2 _ _             _             = typeMismatch

liftSVOp2 :: (String -> String -> String) -> Val -> Val -> Code Val
liftSVOp2 f (StringVal v1) (StringVal v2) = return $ StringVal $ f v1 v2
liftSVOp2 _ _              _              = typeMismatch

liftSVBOp2 :: (String -> String -> Bool) -> Val -> Val -> Code Val
liftSVBOp2 f (StringVal v1) (StringVal v2) = return $ boolToVal $ f v1 v2
liftSVBOp2 _ _             _               = typeMismatch

-- The return (FloatVal 0) will never be executed, but is needed to make the types work
valError :: RuntimeError -> Code Val
valError err = raiseRuntimeError err >> return (FloatVal 0)

typeMismatch, invalidArgument, divisionByZero :: Code Val
typeMismatch    = valError TypeMismatchError
invalidArgument = valError InvalidArgumentError
divisionByZero  = valError DivisionByZeroError

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
eval (ParenX x) = eval x

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
                        else return $ FloatVal $ fv1/fv2
                (_,_) -> typeMismatch
        PowOp -> liftFVOp2 (**)
        EqOp -> \v1 v2 ->
            case (v1,v2) of
                (FloatVal  _, FloatVal  _) -> liftFVBOp2 (==) v1 v2
                (StringVal _, StringVal _) -> liftSVBOp2 (==) v1 v2
                (_,           _          ) -> typeMismatch
        NEOp -> liftFVBOp2 (/=)
        LTOp -> liftFVBOp2 (<)
        LEOp -> liftFVBOp2 (<=)
        GTOp -> liftFVBOp2 (>)
        GEOp -> liftFVBOp2 (>=)
        AndOp -> liftFVOp2 $ \v1 v2 -> if v1/=0 && v2/=0 then v1 else 0
        OrOp -> liftFVOp2 $ \v1 v2 -> if v1/=0 then v1 else v2
-- TO DO: check defined behavior of AND & OR

evalBuiltin :: Builtin -> [Val] -> Code Val
evalBuiltin builtin args = case builtin of
    AbsBI -> liftFVBuiltin1 abs args
    AscBI -> case args of
        [StringVal v] ->
            if length v == 0
            then invalidArgument
            else return $ FloatVal (fromIntegral (fromEnum (head v)))
        _ -> typeMismatch
    AtnBI -> liftFVBuiltin1 atan args
    ChrBI -> case args of
        [FloatVal v] ->
            let iv = floatToInt v in
                if iv < 0 || iv > 255
                then invalidArgument
                else return $ StringVal ([toEnum iv])
        _ -> typeMismatch
    CosBI -> liftFVBuiltin1 cos args
    ExpBI -> liftFVBuiltin1 exp args
    IntBI -> liftFVBuiltin1 (fromIntegral . floatToInt) args
    LeftBI -> case args of
        [StringVal sv, FloatVal fv] ->
            let iv = floatToInt fv in
                return (StringVal (take iv sv))
        _ -> typeMismatch
    LenBI -> case args of
        [StringVal sv] -> return (FloatVal (fromIntegral (length sv)))
        _ -> typeMismatch
    LogBI -> liftFVBuiltin1 log args
    MidBI -> case args of
        [StringVal sv, FloatVal fv] ->
            let iv = floatToInt fv in
                return (StringVal (drop (iv-1) sv))
        [StringVal sv, FloatVal fv1, FloatVal fv2] ->
            let iv1 = floatToInt fv1
                iv2 = floatToInt fv2
            in
                return (StringVal (take iv2 (drop (iv1-1) sv)))
        _ -> typeMismatch
    RightBI -> case args of
        [StringVal sv, FloatVal fv] ->
            let iv = floatToInt fv in
                return (StringVal (drop (length sv - iv) sv))
        _ -> typeMismatch
    RndBI -> case args of
        [FloatVal fv] -> do
            let iv = floatToInt fv
            if iv < 0
                then seedRandom iv
                else return ()
            rv <- if (iv == 0) then getPrevRandom else getRandom
            return (FloatVal rv)
        _ -> typeMismatch
    SgnBI -> liftFVBuiltin1 (\v -> if v < 0 then -1 else if v > 0 then 1 else 0) args
    SinBI -> liftFVBuiltin1 sin args
    SpcBI -> case args of
        [FloatVal fv] ->
            let iv = floatToInt fv in
                return (StringVal (replicate iv ' '))
        _ -> typeMismatch
    SqrBI -> liftFVBuiltin1 sqrt args
    StrBI -> case args of
        [FloatVal fv] -> return (StringVal (showVal (FloatVal fv)))
        _ -> typeMismatch
    TabBI -> case args of
        [FloatVal fv] ->
            let destCol = floatToInt fv in
                if (destCol < 0)
                  then invalidArgument
                  else do
                    curCol <- getOutputColumn
                    return $ StringVal $
                        if curCol > destCol
                          then "\n" ++ replicate destCol ' '
                          else replicate (destCol - curCol) ' '
        _ -> typeMismatch
    TanBI -> liftFVBuiltin1 tan args
    ValBI -> case args of
        [StringVal sv] -> return (FloatVal (maybe 0 id (readFloat sv)))
        _              -> typeMismatch

-- Interpret a tagged statement.
-- Sets the line number in the state, then passes the statement on to interpS.
interpTS :: JumpTable -> Tagged Statement -> Code ()
interpTS jumpTable (Tagged pos statement) = do
    setLineNumber (sourceLine pos)
    interpS jumpTable statement

-- Interpret a single statement.
-- In the type of interpS, the first () signifies what is passed to a
-- resumed trap.  The second one represents what is returned by interpS.
interpS :: JumpTable -> Statement -> Code ()

interpS _ (RemS _) = return ()

interpS _ EndS = end

interpS _ StopS = raiseRuntimeException Suspend

interpS _ (DimS arrs) = mapM_ interpDim arrs

interpS _ (LetS var x) = do
    val <- eval x
    setVar var val

interpS _ (PrintS xs nl) = do
    mapM (\x -> eval x >>= printVal) xs
    -- check for type mismatches
    if nl then printString "\n" else return ()

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
interpS _ (ForS control@(VarName FloatType _) x1 x2 x3) = do
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
                (FloatVal index) <- getScalarVar control
                let index' = index+step
                setScalarVar control (FloatVal index')
                if (step>=0 && index'<=lim)
                    || (step<0 && index>=lim)
                    then continue True
                    else resume False
            else passOn True
interpS _ (ForS _ _ _ _) = raiseRuntimeError TypeMismatchError

interpS _ (NextS Nothing) = raiseRuntimeException (Next Nothing)
interpS _ (NextS (Just vars)) = mapM_ interpNextVar vars

interpS jumpTable (GosubS lab) =
    do let maybeCode = programLookup jumpTable lab
       assert (isJust maybeCode) (BadGosubTargetError lab)
       catchC gosubHandler (fromJust maybeCode)
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

interpNextVar :: VarName -> Code ()
interpNextVar (VarName FloatType v) = raiseRuntimeException (Next (Just v))
interpNextVar _ = raiseRuntimeError TypeMismatchError

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
                    case compare (length vars) (length vals) of
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
getVar (ScalarVar vn) = getScalarVar vn
getVar (ArrVar vn xs) = do
    inds <- mapM eval xs
    is <- checkArrInds inds
    val <- getArrVar vn is
    return $ case val of
        (IntVal iv) -> FloatVal (fromIntegral iv)
        _           -> val

setVar :: Var -> Val -> Code ()
setVar (ScalarVar vn) val = setScalarVar vn val
setVar (ArrVar vn xs) val = do
    inds <- mapM eval xs
    is <- checkArrInds inds
    val' <- coerce vn val
    setArrVar vn is val'

coerce :: Typeable a => a -> Val -> Code Val
coerce var val = case (typeOf var, val) of
    (IntType,    (FloatVal fv)) -> return $ IntVal (floatToInt fv)
    (FloatType,  (FloatVal _ )) -> return val
    (StringType, (StringVal _)) -> return val
    (_,          _)             -> typeMismatch

interpDim :: (VarName, [Expr]) -> Code ()
interpDim (vn, xs) = do
    inds <- mapM eval xs
    is <- checkArrInds inds
    -- add 1, so that range is from 0 to user-specified bound
    let bounds = map (1+) is
    dimArray vn bounds
    return ()

checkArrInds :: [Val] -> Basic (BasicExcep BasicResult ()) [Int]
checkArrInds indVals = do
    indFs <- mapM (extractFloatOrFail TypeMismatchError) indVals
    assert (and (map (>=0) indFs)) NegativeArrayDimError
    let inds = map floatToInt indFs -- round dimensions as per standard
    return inds

-- If Float is a round number, show it as an Int.
showVal :: Val -> String
showVal (FloatVal v) =
    let i = floatToInt v
     in " " ++ (if fromIntegral i == v then show i else show v) ++ " "
showVal (IntVal i) = show i
showVal (StringVal s) = s

printVal :: Val -> Basic o ()
printVal v = printString (showVal v)

dataFromLine :: Line -> [String]
dataFromLine (Line _ stmts) = concat (map (dataFromStatement . getTaggedVal) stmts)

dataFromStatement :: Statement -> [String]
dataFromStatement (DataS s) =
    let (Right ss) = parse dataValsP "" s
    in ss
dataFromStatement _ = []
