{-# OPTIONS_GHC -fglasgow-exts #-}

-- DurableTraps.hs
-- Lyle Kopnicky
-- last updated 2004-09-09

module DurableTraps where

import Control.Monad
import CPST
import ExceptionHandlers

class Eq o => ResultType o where
    okValue :: o

notOK x = x /= okValue

-- A continuation
type Cont o m i = i -> CPST (Excep o m i) m (Excep o m i)

-- Packages a result type with a handler stack and a continuation
data Excep o m i =
    Excep o
          (Excep o m i -> CPST (Excep o m i) m (Excep o m i))
          (Cont o m i)
-- o is the result type (e.g., OK)
-- i is the type to pass to the continuation (bound to the raise)
-- both types must remain the same throughout your CPS computation

-- Packages the current continuation with the exception value,
-- and an empty handler stack
raiseCC :: (Monad m, ResultType o) => o -> CPST (Excep o m i) m i
raiseCC x = callCC (\k -> raise (Excep x return k))

trap :: Monad m =>
        (o
         -> (forall a. Bool -> CPST (Excep o m ()) m a)
         -> (forall a. Bool -> CPST (Excep o m ()) m a)
         -> (forall a. Bool -> CPST (Excep o m ()) m a)
         -> CPST (Excep o m ()) m (Excep o m ()))
        -> CPST (Excep o m ()) m ()

-- This handler provides flexibility.  It takes a function 'f' as a
-- parameter, to which it passes three continuations: passOn, resume,
-- and continue.
--
-- passOn: pass control of the exception on up the handler chain
-- resume: return to where the exception was raised
-- continue: continue with the code following the 'trap' statement
--
-- Each of these continuations is parametrized by a boolean value:
--
-- endure = True: keep this handler in force
-- endure = False: disable this handler
--
-- Local variables:
--
-- h hk - the handler
-- x    - exception (value only)
-- hk   - handler's continuation
-- hc   - handler chain
-- xk   - exception's continuation

trap f = callCC (\hk -> install (h hk))
    where h hk (Excep x hc xk) =
              let hc' exc' = capture (h hk) (hc exc')
                  xk' v  = capture (h hk) (xk v) >>= raise
                  passOn endure =
                      if endure
                         then raise (Excep x hc' xk')
                         else raise (Excep x hc  xk')
                  resume endure =
                      if endure
                         then xk' ()           -- might want other than (),
                         else xk  () >>= raise -- say, for implementing state
                  continue endure =
                      if endure
                         then install hc' >>= hk >>= raise
                         else install hc  >>= hk >>= raise
                  in f x passOn resume continue

-- catchC is the catching version of trap.  It catches exceptions from
-- an expression, instead of from its continuation.

catchC f m = callCC (\hk -> capture (h hk) m)
    where h hk (Excep x hc xk) =
              let hc' exc' = capture (h hk) (hc exc')
                  xk' v  = capture (h hk) (xk v) >>= hk >>= raise
                  passOn endure =
                      if endure
                         then raise (Excep x hc' xk')
                         else raise (Excep x hc  xk')
                  resume endure =
                      if endure
                         then xk' ()           -- might want other than (),
                         else xk  () >>= raise -- say, for implementing state
                  continue endure =
                      if endure
                         then install hc' >> hk (Excep x hc' xk') >>= raise
                         else install hc  >> hk (Excep x hc xk') >>= raise
                  in f x passOn resume continue

-- Like abort, ends in the middle.
end :: (Monad m, ResultType o) => CPST (Excep o m i) m i
end = raiseCC okValue

-- Needs to be added at the end of every do-sequence.  This returns OK
-- with a continuation that just regenerates itself.  It is needed to make
-- the types work out in the sequence.
done :: (Monad m, ResultType o) => CPST (Excep o m i) m (Excep o m i)
done = return (Excep okValue return (\_ -> done))

-- Untypable, since i = Cont o i.
-- In other words, the continuation has to take itself as its argument.
--getcc :: CPST o m (Cont m o i)
--getcc = callCC (\k -> return k)
