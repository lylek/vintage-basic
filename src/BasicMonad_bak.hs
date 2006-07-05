-- BasicMonad.hs
-- Layers CPST over BasicRT to produce the Basic monad.
-- Lyle Kopnicky
-- last updated 2004-09-10

module BasicMonad where

import Control.Monad
import Control.Monad.Trans
import BasicRT
import DurableTraps
import CPST

type Basic o i = CPST o BasicRT i

type BasicExcep o i = Excep o BasicRT i
type BasicCont o i = Cont o BasicRT i

runBasic :: Basic o o -> IO o
runBasic m = runBasicRT (runCPST m)

getFloatV :: String -> Basic o Float
getFloatV v = lift $ getFloatVRT v

setFloatV :: String -> Float -> Basic o ()
setFloatV v fv = lift $ setFloatVRT v fv

getStringV :: String -> Basic o String
getStringV v = lift $ getStringVRT v

setStringV :: String -> String -> Basic o ()
setStringV v s = lift $ setStringVRT v s

printString :: String -> Basic o ()
printString s = lift $ printStringRT s

getString :: Basic o String
getString = lift getStringRT
