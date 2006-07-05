-- TestDurableTraps.hs
-- Lyle Kopnicky
-- last updated 2004-09-03

module Main where

import CPST
import Control.Monad.Trans
import DurableTraps
import Data.IORef

data TestResult = OK | Error1a | Error1b | Error1c | Error2 String
      deriving (Eq, Show)

instance ResultType TestResult where
    okValue = OK

isError2 (Error2 _) = True
isError2 _ = False

put :: String -> CPST o IO ()
put s = liftIO $ putStrLn s

newRef :: a -> CPST o IO (IORef a)
newRef a = liftIO $ newIORef a

setRef :: IORef a -> a -> CPST o IO ()
setRef ref a = liftIO $ writeIORef ref a

getRef :: IORef a -> CPST o IO a
getRef ref = liftIO $ readIORef ref

exec m = do (Excep x _ _) <- runCPST m
            putStr "Result: "
            print x

-- Same as testB5, but in with durable traps.
testE1 = do put "Hello."
            trap $ \ x passOn _ _ ->
                       if x == Error1a
                          then put "Oh happy ending." >> done
                          else passOn False
            trap $ \ x passOn _ _ ->
                       case x of
                              (Error2 msg) -> do put (reverse msg)
                                                 raiseCC Error1a
                                                 done
                              _ -> passOn False
            put "Yeah."
            raiseCC (Error2 "Hello there.")
            put "Goodbye."
            done

-- Demonstrates resumption after a trapped exception.
testE2 = do put "Hello."
            trap $ \ x passOn resume _ ->
                       if x == Error1a
                          then put "That was bad." >> resume False
                          else passOn False
            put "Yeah."
            raiseCC Error1a
            put "Goodbye."
            done

-- On the first trap, the second handler lets the trap "fall through"
-- to the first.  But it tacks itself onto the continuation being passed up,
-- so when the first handler resumes, the second handler is still in place.
testE3 = do put "Hello."
            trap $ \ x passOn resume _ ->
                       if x == Error1a
                          then put "That was bad." >> resume False
                          else passOn False
            put "Yeah."
            trap $ \ x passOn _ _ ->
                       case x of
                              (Error2 msg) -> put (reverse msg) >> done
                              _ -> passOn False
            put "Wow."
            raiseCC Error1a
            put "Yay."
            raiseCC (Error2 "Holy cow.")
            put "Goodbye."
            done

-- This example demonstrates that when a handler calls the resumption
-- continuation, the handler is no longer in place, but is "spent".
testE4 = do put "Hello."
            trap $ \ _ _ resume _ -> put "Error!" >> resume False
            put "Wow."
            raiseCC Error1a
            put "Yay."
            raiseCC Error1a
            put "Goodbye."
            done

-- In this example, the handler puts itself back in place so that when
-- it resumes where the program left off, the handler is still active.
testE5 = do put "Hello."
            trap $ \ x passOn resume _ ->
                       if notOK x
                          then put "Error!" >> resume True
                          else passOn False
            put "Wow."
            raiseCC Error1a
            put "Yay."
            raiseCC Error1a
            put "Goodbye."
            done

-- This demonstrates the persistent trap.  Even through the second trap
-- passes on the Error1a to the first trap, the second trap is not
-- destroyed, but remains in force when the first trap calls 'continue'.
-- Change it to 'passOn False' and the trap won't endure.
testE6 = do bRef <- newRef True
            put "Hello."
            trap $ \ x passOn _ continue ->
                       if x == Error1a
                          then do put "trapped Error1a"
                                  setRef bRef False
                                  continue False
                          else passOn False
            b <- getRef bRef
            if b
               then do trap $ \ x passOn _ _ ->
                                 if x == Error1b
                                    then put "trapped Error1b" >> done
                                    else passOn True
                       raiseCC Error1a
                       done
               else do raiseCC Error1b
                       done

-- This example demonstrates the ordering of durable traps.
-- The second trap endures, even when it is bypassed and the first
-- trap invokes 'continue'.  The second trap is still nested inside
-- the first, so it it first in the chain.
testE7 = do bRef <- newRef True
            put "Hello."
            trap $ \ x passOn _ continue ->
                       if x == Error1a
                          then do put "trapped Error1a"
                                  setRef bRef False
                                  continue True
                          else if x == Error1c
                                  then put "trapped Error1c here" >> done
                                  else passOn False
            b <- getRef bRef
            if b
               then do trap $ \ x passOn _ _ ->
                                 if x == Error1b
                                    then put "trapped Error1b" >> done
                                    else if x == Error1c
                                            then put "trapped Error1c there"
                                                   >> done
                                            else passOn True
                       raiseCC Error1a
                       done
               else do raiseCC Error1c
                       done

-- Main test --

main = exec testE1
