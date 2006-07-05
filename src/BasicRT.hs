-- BasicRT.hs
-- This monad provides runtime support for variable assignment,
-- I/O and a jump table.
-- Lyle Kopnicky
-- last updated 2004-09-15

module BasicRT where

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

defFloatVal = 0 :: Float -- uninitialized float variables
defIntVal = 0 :: Int -- uninitialized int variables
defStringVal = "" -- uninitialized string variables

data BasicStore =
    BasicStore { floatTable :: HashTable String (IORef Float),
            intTable :: HashTable String (IORef Int),
            stringTable :: HashTable String (IORef String),
            -- In the array tables, the [Int] lists the bound of each dimension
            floatArrTable :: HashTable String ([Int], IOArray Int Float),
            intArrTable :: HashTable String ([Int], IOArray Int Int),
            stringArrTable :: HashTable String ([Int], IOArray String String)
          }

data BasicState =
    BasicState { lineNumber :: Int, -- for error reporting
                 outputColumn :: Int -- for TAB() function
               }

type BasicRT = ReaderT BasicStore (StateT BasicState IO)

-- should be called evalInitBasicRT
runBasicRT :: BasicRT a -> IO a
runBasicRT m =
    do ft <- new (==) hashString
       it <- new (==) hashString
       st <- new (==) hashString
       fat <- new (==) hashString
       iat <- new (==) hashString
       sat <- new (==) hashString
       let store = BasicStore ft it st fat iat sat
       evalStateT (runReaderT m store) (BasicState 0 0)

getFloatVRT :: String -> BasicRT Float
getFloatVRT v =
    do store <- ask
       liftIO $ do maybeVarRef <- lookup (floatTable store) v
                   case maybeVarRef
                        of Nothing -> return defFloatVal
                           (Just varRef) -> readIORef varRef

setFloatVRT :: String -> Float -> BasicRT ()
setFloatVRT v fv =
    do store <- ask
       liftIO $ do maybeVarRef <- lookup (floatTable store) v
                   case maybeVarRef
                        of Nothing -> do ref <- newIORef fv
                                         insert (floatTable store) v ref
                           (Just varRef) -> writeIORef varRef fv

getIntVRT :: String -> BasicRT Int
getIntVRT v =
    do store <- ask
       liftIO $ do maybeVarRef <- lookup (intTable store) v
                   case maybeVarRef
                        of Nothing -> return defIntVal
                           (Just varRef) -> readIORef varRef

setIntVRT :: String -> Int -> BasicRT ()
setIntVRT v iv =
    do store <- ask
       liftIO $ do maybeVarRef <- lookup (intTable store) v
                   case maybeVarRef
                        of Nothing -> do ref <- newIORef iv
                                         insert (intTable store) v ref
                           (Just varRef) -> writeIORef varRef iv

getStringVRT :: String -> BasicRT String
getStringVRT v =
    do store <- ask
       liftIO $ do maybeVarRef <- lookup (stringTable store) v
                   case maybeVarRef
                        of Nothing -> return defStringVal
                           (Just varRef) -> readIORef varRef

setStringVRT :: String -> String -> BasicRT ()
setStringVRT v sv =
    do store <- ask
       liftIO $ do maybeVarRef <- lookup (stringTable store) v
                   case maybeVarRef
                        of Nothing -> do ref <- newIORef sv
                                         insert (stringTable store) v ref
                           (Just varRef) -> writeIORef varRef sv

-- BASIC indices range from zero to an upper bound.
-- We're storing 1+ that bound, to make computations easier.
checkIndices bounds indices =
    and $ (zipWith (>) bounds indices) ++ (map (>=0) indices)

arrIndex bounds indices =
    let scales = tail $ scanr (*) 1 bounds
        in sum $ zipWith (*) scales indices

-- This function doesn't distinguish between mismatched number of indices
-- (statically checkable) and out-of-bounds errors (run-time only).
-- Perhaps this function should be moved to the Basic monad?
getFloatArrVRT :: String -> [Int] -> BasicRT (Maybe Float)
getFloatArrVRT v indices =
    do store <- ask
       liftIO $ do maybeArray <- lookup (floatArrTable store) v
                   case maybeArray
                        of Nothing -> return Nothing
                           (Just (bounds, arr))
                               -> if length bounds /= length indices
                                     || not (checkIndices bounds indices)
                                     then return Nothing
                                     else do fv <- readArray arr
                                                      (arrIndex bounds indices)
                                             return (Just fv)

printStringRT :: String -> BasicRT ()
printStringRT s = liftIO (putStr s)

getStringRT :: BasicRT String
getStringRT = liftIO getLine

-- A sample for testing the monad.

sampleBas1 :: BasicRT ()
sampleBas1 = do setStringVRT "AB" "Hello there!\n"
                s <- getStringVRT "AB"
                printStringRT s

testBas1 = runBasicRT sampleBas1
