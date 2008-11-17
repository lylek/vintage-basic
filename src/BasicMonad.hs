{-# LANGUAGE FlexibleContexts, Rank2Types #-}

-- BasicMonad.hs
-- This monad provides runtime support for variable assignment,
-- I/O and a jump table.
-- Lyle Kopnicky

module BasicMonad where

import Prelude hiding (lookup)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.HashTable
import Data.IORef
import Data.Array.IO
import Data.Maybe
import Data.Time
import System.IO
import System.Random
import BasicPrinter(printVarName)
import BasicResult
import BasicSyntax
import CPST
import DurableTraps

data Val = FloatVal Float | IntVal Int | StringVal String
    deriving (Eq,Show,Ord)

instance Typeable Val where
    typeOf (FloatVal  _) = FloatType
    typeOf (IntVal    _) = IntType
    typeOf (StringVal _) = StringType

defVal :: ValType -> Val
defVal FloatType  = FloatVal 0
defVal IntType    = IntVal 0
defVal StringType = StringVal ""

data BasicStore = BasicStore {
    scalarTable :: HashTable VarName (IORef Val),
    -- In arrTable, [Int] lists the bound of each dimension
    arrayTable :: HashTable VarName ([Int], IOArray Int Val),
    fnTable :: HashTable VarName (IORef ([Val] -> Code Val))
}

defBounds :: [Int]
defBounds = [11] -- default array bounds: one dimension, 0-10

floatToInt :: Float -> Int
floatToInt = floor

data BasicState = BasicState {
    lineNumber :: Int, -- for error reporting
    outputColumn :: Int, -- for TAB() function
    prevRandomVal :: Float,
    randomGen :: StdGen,
    dataStrings :: [String]
}

type BasicRT = ReaderT BasicStore (StateT BasicState IO)
type Basic o = CPST o BasicRT
type BasicCont o i = Cont o BasicRT i
type BasicExcep o i = Excep o BasicRT i
type Code a = Basic (BasicExcep BasicResult ()) a
type Program = Code (BasicExcep BasicResult ())
type BasicExceptionHandler = ExceptionHandler BasicResult BasicRT

errorDumper :: BasicExceptionHandler
errorDumper x _ _ continue = do
    if x == okValue
       then return ()
       else do
           state <- get
           printString (show x ++ " IN LINE " ++ show (lineNumber state) ++ "\n")
    continue False

runProgram :: Program -> IO ()
runProgram prog = do
    hFlush stdout
    runBasic (catchC errorDumper prog)
    return ()

runBasic :: Basic o o -> IO (o,BasicState)
runBasic m = do
    st <- new (==) (hashString . printVarName)
    at <- new (==) (hashString . printVarName)
    ft <- new (==) (hashString . printVarName)
    let store = BasicStore st at ft
    runStateT (runReaderT (runCPST m) store) (BasicState 0 0 0 (mkStdGen 0) [])

assert :: Bool -> String -> Code ()
assert cond err = if cond then return () else basicError err

basicError :: String -> Code ()
basicError err = raiseCC (Fail err) >> return ()

extractFloatOrFail :: String -> Val -> Code Float
extractFloatOrFail _   (FloatVal fv) = return fv
extractFloatOrFail err _             = basicError err >> return 0

getScalarVar :: VarName -> Basic o Val
getScalarVar vn = do
    store <- ask
    liftIO $ do
        maybeVarRef <- lookup (scalarTable store) vn
        case maybeVarRef of
            Nothing -> return (defVal (typeOf vn))
            (Just varRef) -> readIORef varRef

setScalarVar :: VarName -> Val -> Basic o ()
setScalarVar vn val = do
    store <- ask
    liftIO $ do
        maybeVarRef <- lookup (scalarTable store) vn
        case maybeVarRef of
            Nothing -> do
                ref <- newIORef val
                insert (scalarTable store) vn ref
            (Just varRef) -> writeIORef varRef val

-- BASIC indices range from zero to an upper bound.
-- We're storing 1+ that bound, to make computations easier.
indicesAreWithinBounds :: [Int] -> [Int] -> Bool
indicesAreWithinBounds bounds indices =
    and $ (zipWith (>) bounds indices) ++ (map (>=0) indices)

arrIndex :: [Int] -> [Int] -> Int
arrIndex bounds indices =
    let scales = tail $ scanr (*) 1 bounds
        in sum $ zipWith (*) scales indices

dimArray :: VarName -> [Int] -> Code ([Int], (IOArray Int Val))
dimArray vn bounds = do
    store <- ask
    maybeArray <- liftIO $ lookup (arrayTable store) vn
    assert (isNothing maybeArray) "!REDIM'D ARRAY ERROR"
    arr <- liftIO $ newArray (0, product bounds - 1) (defVal (typeOf vn))
    liftIO $ insert (arrayTable store) vn (bounds, arr)
    return (bounds, arr)

lookupArray :: VarName -> [Int] -> Code ([Int], (IOArray Int Val))
lookupArray vn indices = do
    store <- ask
    maybeArray <- liftIO $ lookup (arrayTable store) vn
    (bounds, arr) <- case maybeArray of
        Nothing -> dimArray vn defBounds
        (Just (bounds, arr)) -> return (bounds, arr)
    assert (length bounds == length indices) "!MISMATCHED ARRAY DIMENSIONS"
    assert (indicesAreWithinBounds bounds indices) "!OUT OF ARRAY BOUNDS"
    return (bounds, arr)

getArrVar :: VarName -> [Int] -> Code Val
getArrVar vn indices = do
    (bounds, arr) <- lookupArray vn indices
    liftIO $ readArray arr (arrIndex bounds indices)

setArrVar :: VarName -> [Int] -> Val -> Code ()
setArrVar vn indices val = do
    (bounds, arr) <- lookupArray vn indices
    liftIO $ writeArray arr (arrIndex bounds indices) val

getFn :: VarName -> Code ([Val] -> Code Val)
getFn vn = do
    store <- ask
    maybeVarRef <- liftIO $ lookup (fnTable store) vn
    assert (isJust maybeVarRef) ("!UNDEFINED FUNCTION " ++ printVarName vn)
    liftIO $ readIORef (fromJust maybeVarRef)

setFn :: VarName -> ([Val] -> Code Val) -> Code ()
setFn vn fn = do
    store <- ask
    liftIO $ do
        maybeVarRef <- lookup (fnTable store) vn
        case maybeVarRef of
            Nothing -> do
                ref <- newIORef fn
                insert (fnTable store) vn ref
            (Just varRef) -> writeIORef varRef fn

setLineNumber :: Int -> Basic o ()
setLineNumber lineNum = modify (\state -> state { lineNumber = lineNum })

zoneWidth :: Int
zoneWidth = 14

printString :: String -> Basic o ()
printString s = do
    state <- get
    let startCol = outputColumn state
    liftIO $ putStr s >> hFlush stdout
    put (state { outputColumn = endCol startCol s })

endCol :: Int -> String -> Int    
endCol startCol "" = startCol
endCol _ ('\n' : s) = endCol 0 s
endCol startCol (_ : s) = endCol (startCol + 1) s

getOutputColumn :: Code Int
getOutputColumn = do
    state <- get
    return $ outputColumn state

getString :: Basic o String
getString = liftIO $ hFlush stdout >> getLine

secondsSinceMidnight :: Code Int
secondsSinceMidnight = do
    zonedTime <- liftIO getZonedTime
    return (floor $ toRational $ timeOfDayToTime $ localTimeOfDay $
      zonedTimeToLocalTime zonedTime)

seedRandom :: Int -> Code ()
seedRandom i = modify (\state -> state { randomGen = (mkStdGen i) })

seedRandomFromTime :: Code ()
seedRandomFromTime = secondsSinceMidnight >>= seedRandom

getPrevRandom :: Code Float
getPrevRandom = do
    state <- get
    return (prevRandomVal state)

getRandom :: Code Float
getRandom = do
    state <- get
    let (rVal, rGen) = random (randomGen state)
    put (state { prevRandomVal = rVal, randomGen = rGen })
    return rVal

setDataStrings :: [String] -> Code ()
setDataStrings ds = modify (\store -> store { dataStrings = ds })

readData :: Code String
readData = do
    state <- get
    let ds = dataStrings state
    assert (not (null ds)) "!OUT OF DATA"
    put (state { dataStrings = tail ds })
    return (head ds)
