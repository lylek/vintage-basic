{-# OPTIONS_GHC -fglasgow-exts #-}

-- BasicMonad.hs
-- This monad provides runtime support for variable assignment,
-- I/O and a jump table.
-- Lyle Kopnicky

module BasicMonad where

import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.HashTable
import Data.IORef
import Data.Array.MArray
import Data.Array.IO
import Data.Ix
import Data.Maybe
import System.IO
import System.Random
import System.Time
import BasicResult
import CPST
import CPSTInstances
import DurableTraps

data BasicStore = BasicStore {
    floatTable :: HashTable String (IORef Float),
    intTable :: HashTable String (IORef Int),
    stringTable :: HashTable String (IORef String),
    -- In the array tables, the [Int] lists the bound of each dimension
    floatArrTable :: HashTable String ([Int], IOArray Int Float),
    intArrTable :: HashTable String ([Int], IOArray Int Int),
    stringArrTable :: HashTable String ([Int], IOArray Int String)
}

class BasicType a where
    defVal :: a
    scalarTable :: BasicStore -> HashTable String (IORef a)
    arrayTable :: BasicStore -> HashTable String ([Int], IOArray Int a)
    constrainArray :: ([Int], IOArray Int a) -> a -> ([Int], IOArray Int a)
    constrainArray (bounds, arr) typ = (bounds, arr)

-- For debugging, to "comment something out"
ignore x = return ()

defBounds = [11] :: [Int] -- default array bounds: one dimension, 0-10

floatToInt :: Float -> Int
floatToInt = round

instance BasicType Float where
    defVal = 0
    scalarTable = floatTable
    arrayTable = floatArrTable

instance BasicType Int where
    defVal = 0
    scalarTable = intTable
    arrayTable = intArrTable

instance BasicType String where
    defVal = ""
    scalarTable = stringTable
    arrayTable = stringArrTable

data BasicState = BasicState {
    lineNumber :: Int, -- for error reporting
    outputColumn :: Int, -- for TAB() function
    prevRandomVal :: Float,
    randomGen :: StdGen
}

type BasicRT = ReaderT BasicStore (StateT BasicState IO)
type Basic o = CPST o BasicRT
type BasicCont o i = Cont o BasicRT i
type BasicExcep o i = Excep o BasicRT i
type Code a = Basic (BasicExcep BasicResult ()) a
type Program = Code (BasicExcep BasicResult ())

runProgram :: Program -> IO ()
runProgram prog = do
    let errorDumper x (passOn :: Bool -> Code ()) resume continue = do
            if x == okValue
               then return ()
               else do
                   state <- get
                   printString (show x ++ " IN LINE " ++ show (lineNumber state))
            continue False
    hFlush stdout
    runBasic (catchC errorDumper prog)
    return ()

runBasic :: Basic o o -> IO (o,BasicState)
runBasic m = do
    ft <- new (==) hashString
    it <- new (==) hashString
    st <- new (==) hashString
    fat <- new (==) hashString
    iat <- new (==) hashString
    sat <- new (==) hashString
    let store = BasicStore ft it st fat iat sat
    runStateT (runReaderT (runCPST m) store) (BasicState 0 0 0 (mkStdGen 0))

assert cond err = if cond then return () else basicError err

basicError :: String -> Code ()
basicError err = raiseCC (Fail err) >> return ()

getVar :: BasicType a => String -> Basic o a
getVar v = do
    store <- ask
    liftIO $ do
        maybeVarRef <- lookup (scalarTable store) v
        case maybeVarRef of
            Nothing -> return defVal
            (Just varRef) -> readIORef varRef

setVar :: BasicType a => String -> a -> Basic o ()
setVar var val = do
    store <- ask
    liftIO $ do
        maybeVarRef <- lookup (scalarTable store) var
        case maybeVarRef of
            Nothing -> do
                ref <- newIORef val
                insert (scalarTable store) var ref
            (Just varRef) -> writeIORef varRef val

-- BASIC indices range from zero to an upper bound.
-- We're storing 1+ that bound, to make computations easier.
checkIndices bounds indices =
    and $ (zipWith (>) bounds indices) ++ (map (>=0) indices)

arrIndex bounds indices =
    let scales = tail $ scanr (*) 1 bounds
        in sum $ zipWith (*) scales indices

-- Creates a new array.  How does it know what type?  The t
-- dummy variable called 'typ'.  The constrainArray function is never
-- called - it is just used to create a type restriction.
dimArray :: BasicType a => String -> [Int] -> a -> Code ([Int], (IOArray Int a))
dimArray var bounds typ = do
    store <- ask
    maybeArray <- liftIO $ lookup (arrayTable store) var
    ignore $ constrainArray (fromJust maybeArray) typ
    assert (isNothing maybeArray) "!REDIM'D ARRAY ERROR"
    arr <- liftIO $ newArray (0,product bounds - 1) defVal
    ignore $ constrainArray (bounds, arr) typ
    liftIO $ insert (arrayTable store) var (bounds, arr)
    return (bounds, arr)

lookupArray :: BasicType a => String -> [Int] -> Code ([Int], (IOArray Int a))
lookupArray var indices = do
    store <- ask
    maybeArray <- liftIO $ lookup (arrayTable store) var
    --assert (isJust maybeArray) "!ARRAY NOT DIM'D"
    (bounds, arr) <- case maybeArray of
        Nothing -> dimArray var defBounds defVal
        (Just (bounds, arr)) -> return (bounds, arr)
    assert (length bounds == length indices) "!MISMATCHED ARRAY DIMENSIONS"
    assert (checkIndices bounds indices) "!OUT OF ARRAY BOUNDS"
    return (bounds, arr)

-- Should auto-dim an array for a standard size if it doesn't exist.
getArr :: BasicType a => String -> [Int] -> Code a
getArr var indices = do
    (bounds, arr) <- lookupArray var indices
    liftIO $ readArray arr (arrIndex bounds indices)

-- Should auto-dim an array for a standard size if it doesn't exist.
setArr :: BasicType a => String -> [Int] -> a -> Code ()
setArr var indices val = do
    (bounds, arr) <- lookupArray var indices
    liftIO $ writeArray arr (arrIndex bounds indices) val

setLineNumber :: Int -> Basic o ()
setLineNumber lineNum = modify (\state -> state { lineNumber = lineNum })

zoneWidth = 14 :: Int

printString :: String -> Basic o ()
printString s = do
    state <- get
    let startCol = outputColumn state
    liftIO $ putStr s >> hFlush stdout
    put (state { outputColumn = endCol startCol s })

endCol :: Int -> String -> Int    
endCol startCol "" = startCol
endCol startCol ('\n' : s) = endCol 0 s
endCol startCol (c : s) = endCol (startCol + 1) s

getOutputColumn :: Code Int
getOutputColumn = do
    state <- get
    return $ outputColumn state

getString :: Basic o String
getString = liftIO $ hFlush stdout >> getLine

secondsSinceMidnight :: Code Int
secondsSinceMidnight = do
    clockTime <- liftIO $ getClockTime
    calendarTime <- liftIO $ toCalendarTime clockTime
    return ((ctHour calendarTime * 24 + ctMin calendarTime) * 60 + ctSec calendarTime)

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

{-
-- A sample for testing the monad.

sampleBas1 :: ResultType o => Basic (Excep o BasicRT i) (Excep o BasicRT i)
sampleBas1 = do setStringV "AB" "Hello there!\n"
                s <- getStringV "AB"
                printString s
                done

testBas1 :: ResultType o => IO (Excep o BasicRT i)
testBas1 = runBasic sampleBas1
-}
