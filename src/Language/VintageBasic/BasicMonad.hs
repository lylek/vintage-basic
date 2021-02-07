{-# LANGUAGE FlexibleContexts, Rank2Types #-}

-- | This monad provides runtime support for variable assignment,
-- user-defined functions, I/O, and a jump table.

module Language.VintageBasic.BasicMonad where

import Prelude hiding (lookup)
import Control.Monad.CPST
import Control.Monad.CPST.DurableTraps
import Control.Monad.Reader
import Control.Monad.State (StateT, runStateT, get, put, modify)
import Data.HashTable.IO
import Data.IORef
import Data.Array.IO
import Data.Maybe
import Data.Time
import IO.IOStream
import Language.VintageBasic.Result
import Language.VintageBasic.Syntax
import System.Random

-- | Values in BASIC programs.
data Val = FloatVal Float | IntVal Int | StringVal String
    deriving (Eq,Show,Ord)

instance Typeable Val where
    typeOf (FloatVal  _) = FloatType
    typeOf (IntVal    _) = IntType
    typeOf (StringVal _) = StringType

-- | The default variable value (before assignment) for each BASIC type.
defVal :: ValType -> Val
defVal FloatType  = FloatVal 0
defVal IntType    = IntVal 0
defVal StringType = StringVal ""

-- | The store of BASIC variables.
data BasicStore = BasicStore {
    scalarTable :: BasicHashTable VarName (IORef Val),
      -- ^ scalar variable assignments
    arrayTable :: BasicHashTable VarName ([Int], IOArray Int Val),
      -- ^ array variable values (@[Int]@ lists the bound of each dimension)
    fnTable :: BasicHashTable VarName (IORef ([Val] -> Code Val))
      -- ^ user-defined function (@DEF FN@) definitions
}

-- | The default array bounds.
defBounds :: [Int]
defBounds = [11] -- one dimension, 0-10

-- | The common way to convert Float to Int values in BASIC is to round down.
floatToInt :: Float -> Int
floatToInt = floor

data BasicState = BasicState {
    inputStream :: IOStream,  -- ^ where @INPUT@ comes from
    outputStream :: IOStream, -- ^ where @PRINT@ output goes
    lineNumber :: Int,        -- ^ for error reporting
    outputColumn :: Int,      -- ^ for the @TAB()@ function
    prevRandomVal :: Float,   -- ^ the previously generated random value
    randomGen :: StdGen,      -- ^ random number generator
    dataStrings :: [String]   -- ^ the strings extracted from all @DATA@ statements
}

type BasicRT = ReaderT BasicStore (StateT BasicState IO)
type Basic o = CPST o BasicRT
type BasicCont o i = Cont o BasicRT i
type BasicExcep o i = Excep o BasicRT i
type Code a = Basic (BasicExcep Result ()) a
type Program = Code (BasicExcep Result ())
type BasicExceptionHandler = ExceptionHandler Result BasicRT

-- | An exception handler used to print error messages at the outermost
-- level of execution.
errorDumper :: BasicExceptionHandler
errorDumper x _ _ continue = do
    case x of
        Pass -> return ()
        _    -> printString $ (show x ++ "\n")
    continue False

-- | Runs an interpreted BASIC program using the supplied input and output streams.
runProgram
    :: IOStream -- ^ input stream
    -> IOStream -- ^ output stream
    -> Program  -- ^ the BASIC program, in lazy interpreted form
    -> IO ()
runProgram inputHandle outputHandle prog = do
    vFlush outputHandle
    st <- new
    at <- new
    ft <- new
    let store = BasicStore st at ft
    let state = (BasicState inputHandle outputHandle 0 0 0 (mkStdGen 0) [])
    _ <- runStateT (runReaderT (runCPST (catchC errorDumper prog)) store) state
    return ()

-- | Raises a runtime error if the assertion is False.
assert :: Bool -> RuntimeError -> Code ()
assert cond err = if cond then return () else raiseRuntimeError err

raiseRuntimeException :: RuntimeException -> Code ()
raiseRuntimeException rte = do
    state <- get
    raiseCC (LabeledRuntimeException (lineNumber state) rte)
    return ()

raiseRuntimeError :: RuntimeError -> Code ()
raiseRuntimeError err = raiseRuntimeException (RuntimeError err)

extractFloatOrFail :: RuntimeError -> Val -> Code Float
extractFloatOrFail _   (FloatVal fv) = return fv
extractFloatOrFail err _             = raiseRuntimeError err >> return 0

-- | Get the value of a scalar variable.
getScalarVar :: VarName -> Basic o Val
getScalarVar vn = do
    store <- ask
    liftIO $ do
        maybeVarRef <- lookup (scalarTable store) vn
        case maybeVarRef of
            Nothing -> return (defVal (typeOf vn))
            (Just varRef) -> readIORef varRef

-- | Set the value of a scalar variable.
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

-- | BASIC indices range from zero to an upper bound.
-- We're storing 1+ that bound, to make computations easier.
indicesAreWithinBounds :: [Int] -> [Int] -> Bool
indicesAreWithinBounds bounds indices =
    and $ (zipWith (>) bounds indices) ++ (map (>=0) indices)

-- | Given the array's bounds and indices, compute the linear index
-- into the storage array.
arrIndex :: [Int] -> [Int] -> Int
arrIndex bounds indices =
    let scales = tail $ scanr (*) 1 bounds
        in sum $ zipWith (*) scales indices

-- | Create a new array in the store, with the specified name and bounds.
dimArray :: VarName -> [Int] -> Code ([Int], (IOArray Int Val))
dimArray vn bounds = do
    store <- ask
    maybeArray <- liftIO $ lookup (arrayTable store) vn
    assert (isNothing maybeArray) ReDimensionedArrayError
    arr <- liftIO $ newArray (0, product bounds - 1) (defVal (typeOf vn))
    liftIO $ insert (arrayTable store) vn (bounds, arr)
    return (bounds, arr)

-- | Look up an array from the store. Indices are provided just to check
-- validity.
lookupArray :: VarName -> [Int] -> Code ([Int], (IOArray Int Val))
lookupArray vn indices = do
    store <- ask
    maybeArray <- liftIO $ lookup (arrayTable store) vn
    (bounds, arr) <- case maybeArray of
        Nothing -> dimArray vn defBounds
        (Just (bounds, arr)) -> return (bounds, arr)
    assert (length bounds == length indices) MismatchedArrayDimensionsError
    assert (indicesAreWithinBounds bounds indices) OutOfArrayBoundsError
    return (bounds, arr)

-- | Get a value from an array variable.
getArrVar :: VarName -> [Int] -> Code Val
getArrVar vn indices = do
    (bounds, arr) <- lookupArray vn indices
    liftIO $ readArray arr (arrIndex bounds indices)

-- | Set a value in an array variable.
setArrVar :: VarName -> [Int] -> Val -> Code ()
setArrVar vn indices val = do
    (bounds, arr) <- lookupArray vn indices
    liftIO $ writeArray arr (arrIndex bounds indices) val

-- | Retrieve the code for a user-defined function from the store.
getFn :: VarName -> Code ([Val] -> Code Val)
getFn vn = do
    store <- ask
    maybeVarRef <- liftIO $ lookup (fnTable store) vn
    assert (isJust maybeVarRef) (UndefinedFunctionError vn)
    liftIO $ readIORef (fromJust maybeVarRef)

-- | Define a user-defined function in the store.
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

-- | Set the current line number in the BASIC state.
setLineNumber :: Int -> Basic o ()
setLineNumber lineNum = do
    state <- get
    seq state $ put (state { lineNumber = lineNum })

-- | The width of print zones, used for commas in a @PRINT@ statement.
zoneWidth :: Int
zoneWidth = 14

-- | Output a string, updating the current output column in the state.
printString :: String -> Basic o ()
printString s = do
    state <- get
    let startCol = outputColumn state
    liftIO $ vPutStr (outputStream state) s >> vFlush (outputStream state)
    put (state { outputColumn = endCol startCol s })

-- | Given a starting column and a string, returns the ending column,
-- were that string to be output.
endCol :: Int -> String -> Int    
endCol startCol "" = startCol
endCol _ ('\n' : s) = endCol 0 s
endCol _ ('\r' : s) = endCol 0 s
endCol startCol (_ : s) = endCol (startCol + 1) s

-- | Retrieves the current output column from the state.
getOutputColumn :: Code Int
getOutputColumn = do
    state <- get
    return $ outputColumn state

-- | Gets an input line, raising an exception if we are at EOF.
getString :: Code String
getString = do
    state <- get
    liftIO $ vFlush (outputStream state)
    eof <- liftIO $ vIsEOF (inputStream state)
    assert (not eof) EndOfInputError
    str <- liftIO $ vGetLine (inputStream state)
    put state { outputColumn = 0 }
    return str

-- | The number of seconds that have elapsed since midnight (local time).
secondsSinceMidnight :: Code Int
secondsSinceMidnight = do
    zonedTime <- liftIO getZonedTime
    return (floor $ toRational $ timeOfDayToTime $ localTimeOfDay $
      zonedTimeToLocalTime zonedTime)

-- | Seeds the BASIC random number generator using an integer.
seedRandom :: Int -> Code ()
seedRandom i = modify (\state -> state { randomGen = (mkStdGen i) })

-- | Seeds the BASIC random number generator based on the number of
-- seconds since midnight.
seedRandomFromTime :: Code ()
seedRandomFromTime = secondsSinceMidnight >>= seedRandom

-- | Get the previously generated random number from the state.
-- Used for @RND(0)@.
getPrevRandom :: Code Float
getPrevRandom = do
    state <- get
    return (prevRandomVal state)

-- | Generate a new random number.
getRandom :: Code Float
getRandom = do
    state <- get
    let (rVal, rGen) = random (randomGen state)
    put (state { prevRandomVal = rVal, randomGen = rGen })
    return rVal

-- | Set the list of @DATA@ statement strings in the store.
setDataStrings :: [String] -> Code ()
setDataStrings ds = modify (\store -> store { dataStrings = ds })

-- | @READ@ the next @DATA@ value from the data strings.
readData :: Code String
readData = do
    state <- get
    let ds = dataStrings state
    assert (not (null ds)) OutOfDataError
    put (state { dataStrings = tail ds })
    return (head ds)
