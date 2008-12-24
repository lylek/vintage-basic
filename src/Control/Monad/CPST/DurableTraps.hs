{-# LANGUAGE FlexibleContexts, RankNTypes #-}

-- DurableTraps.hs
-- Lyle Kopnicky

module Control.Monad.CPST.DurableTraps where

import Control.Monad
import Control.Monad.CPST
import Control.Monad.CPST.ExceptionHandlers(capture,install,raise)

class ResultType o where
    okValue :: o

-- | A continuation
type Cont o m i = i -> CPST (Excep o m i) m (Excep o m i)

-- | Packages a result type with a handler stack and a continuation
data Excep o m i =
    Excep o
          (Excep o m i -> CPST (Excep o m i) m (Excep o m i))
          (Cont o m i)
-- o is the result type (e.g., OK)
-- i is the type to pass to the continuation (bound to the raise)
-- both types must remain the same throughout your CPS computation

-- | Packages the current continuation with the exception value,
-- and an empty handler stack
raiseCC :: (Monad m, ResultType o) => o -> CPST (Excep o m i) m i
raiseCC x = callCC (\k -> raise (Excep x return k))

type ExceptionResumer o m = forall a. Bool -> CPST (Excep o m ()) m a

type ExceptionHandler o m = Monad m => o -> ExceptionResumer o m -> ExceptionResumer o m
    -> ExceptionResumer o m -> CPST (Excep o m ()) m (Excep o m ())

trap :: Monad m => ExceptionHandler o m -> CPST (Excep o m ()) m ()

-- | This handler provides flexibility.  It takes a function 'f' as a
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

-- | The catching version of trap.  It catches exceptions from
-- an expression, instead of from its continuation.

catchC :: Monad m =>
    ExceptionHandler o m
    -> CPST (Excep o m ()) m (Excep o m ())
    -> CPST (Excep o m ()) m (Excep o m ())

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

-- | Like abort, ends in the middle of a program.  Can be resumed.
end :: (Monad m, ResultType o) => CPST (Excep o m i) m i
end = raiseCC okValue

-- | Needs to be added at the end of every do-sequence.  This returns OK
-- with a continuation that just regenerates itself.  It is needed to make
-- the types work out in the sequence.
done :: (Monad m, ResultType o) => CPST (Excep o m i) m (Excep o m i)
done = return (Excep okValue return (\_ -> done))

-- | Raises an exception that can't be resumed, and can be used in any context.
die :: (Monad m, ResultType o) => o -> CPST (Excep o m i) m a
die x = raise (Excep x return (error "cannot resume from a die"))

-- Untypable, since i = Cont o i.
-- In other words, the continuation has to take itself as its argument.
--getcc :: CPST o m (Cont m o i)
--getcc = callCC (\k -> return k)
