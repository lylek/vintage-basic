{-# LANGUAGE FlexibleContexts, RankNTypes #-}

-- | Support for advanced exception handling within the CPST monad.

module Control.Monad.CPST.DurableTraps where

import Control.Monad.CPST
import Control.Monad.CPST.ExceptionHandlers(capture,install,raise)

class ResultType o where
    okValue :: o

-- | A continuation in the CPST monad, for code that may
-- throw exceptions.
type Cont o m i = i -> CPST (Excep o m i) m (Excep o m i)

-- | The full-blown exception including not only an exception value,
-- but a handler stack and continuation. Having all this allows
-- an exception handler to manipulate the handler stack or resume
-- using the exception's continuation.
--
-- [@o@] is the result type (e.g., OK, or an exception value)
--
-- [@i@] is the type to pass to the continuation (bound to the raise)
--
-- Both types must remain the same throughout your CPS computation.
data Excep o m i =
    Excep { exceptionValue        :: o
          , exceptionHandlerStack :: (Excep o m i -> CPST (Excep o m i) m (Excep o m i))
          , exceptionContinuation :: (Cont o m i)
          }

-- | Packages the current continuation with the exception value,
-- and an empty handler stack.
raiseCC :: (Monad m, ResultType o) => o -> CPST (Excep o m i) m i
raiseCC x = callCC (\k -> raise (Excep x return k))

type ExceptionResumer o m = forall a. Bool -> CPST (Excep o m ()) m a

type ExceptionHandler o m = Monad m => o -> ExceptionResumer o m -> ExceptionResumer o m
    -> ExceptionResumer o m -> CPST (Excep o m ()) m (Excep o m ())

-- | A flexible exception handling mechanism.  It takes a function 'f' as a
-- parameter, to which it passes three continuations: @passOn@, @resume@,
-- and @continue@.
--
-- [@passOn@] pass control of the exception on up the handler chain (re-raise)
--
-- [@resume@] return to where the exception was raised (the exception's continuation)
--
-- [@continue@] continue with the code following the 'trap' statement
--     (the handler's continuation)
--
-- Each of these continuations is parametrized by a boolean value:
--
-- [@endure = True@] keep this handler in force
--
-- [@endure = False@] disable this handler (remove it from the chain)

trap :: Monad m
    => ExceptionHandler o m     -- ^ exception handler
    -> CPST (Excep o m ()) m ()

-- Local variables:
--
-- [@h hk@] the full handler with all logic
--
-- [@xv  @] exception value
--
-- [@hk  @] handler's continuation
--
-- [@hc  @] handler chain
--
-- [@xk  @] exception's continuation

trap f = callCC (\hk -> install (h hk))
    where h hk (Excep xv hc xk) =
              let hc' exc' = capture (h hk) (hc exc')
                  xk' v  = capture (h hk) (xk v)
                  passOn endure =
                      if endure
                         then raise (Excep xv hc' xk')
                         else raise (Excep xv hc  xk')
                  resume endure =
                      if endure
                         then xk' () >>= raise -- might want other than (),
                         else xk  () >>= raise -- say, for implementing state
                  continue endure =
                      if endure
                         then install hc' >>= hk >>= raise
                         else install hc  >>= hk >>= raise
                  in f xv passOn resume continue

-- | The catching version of trap.  It catches exceptions from
-- an expression, instead of from its continuation.

catchC :: Monad m
    => ExceptionHandler o m                 -- ^ exception handler
    -> CPST (Excep o m ()) m (Excep o m ()) -- ^ computation in which to catch exceptions
    -> CPST (Excep o m ()) m (Excep o m ())

catchC f m = callCC (\hk -> capture (h hk) m)
    where h hk (Excep xv hc xk) =
              let hc' exc' = capture (h hk) (hc exc')
                  xk' v  = capture (h hk) (xk v) >>= hk
                  passOn endure =
                      if endure
                         then raise (Excep xv hc' xk')
                         else raise (Excep xv hc  xk')
                  resume endure =
                      if endure
                         then xk' () >>= raise -- might want other than (),
                         else xk  () >>= raise -- say, for implementing state
                  continue endure =
                      if endure
                         then install hc' >> hk (Excep xv hc' xk') >>= raise
                         else install hc  >> hk (Excep xv hc xk') >>= raise
                  in f xv passOn resume continue

-- | Like abort, ends in the middle of a program.  Can be resumed.
end :: (Monad m, ResultType o) => CPST (Excep o m i) m i
end = raiseCC okValue

-- | Can be added at the end of a @do@-sequence to make the return type
-- match the ultimate return type. It returns OK with a continuation that
-- regenerates itself.
done :: (Monad m, ResultType o) => CPST (Excep o m i) m (Excep o m i)
done = return (Excep okValue return (\_ -> done))

-- | Raises an exception that can't be resumed, and can be used in any context.
die :: (Monad m, ResultType o) => o -> CPST (Excep o m i) m a
die x = raise (Excep x return (error "cannot resume from a die"))

-- Note: getCC might seem like a nice idea, but it's untypable, since the continuation
-- has to take itself as an argument. An incorrect attempt:
--
-- getcc :: CPST o m (Cont m o i)
-- getcc = callCC (\k -> return k)
