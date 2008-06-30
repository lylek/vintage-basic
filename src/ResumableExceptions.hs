-- ResumableExceptions.hs
-- Lyle Kopnicky

module ResumableExceptions where

import Control.Monad
import CPST
import ExceptionHandlers

class Eq o => ResultType o where
    okValue :: o

notOK x = x /= okValue

-- A continuation
type Cont o m i = i -> CPST (Excep o m i) m (Excep o m i)

-- Packages a result type with a continuation
data Excep o m i = Excep o (Cont o m i)
-- o is the result type (e.g., OK)
-- i is the type to pass to the continuation (bound to the raise)
-- both types must remain the same throughout your CPS computation

-- Packages the current continuation with the exception value
raiseCC :: (Monad m, ResultType o) => o -> CPST (Excep o m i) m i
raiseCC x = callCC (\k -> raise (Excep x k))

-- If this handler doesn't process the exception, passes it on up the line -
-- at the same time adding itself to the continuation.  The handling function
-- you pass it gets three arguments: the exception, the continuation,
-- and the handler itself, in case it wants to reuse it.

catchC :: (Monad m, ResultType o) =>
          (o -> Bool)
              -> (o
                  -> Cont o m i
                  -> (Excep o m i
                      -> CPST (Excep o m i) m (Excep o m i))
                  -> CPST (Excep o m i) m (Excep o m i))
              -> CPST (Excep o m i) m (Excep o m i)
              -> CPST (Excep o m i) m (Excep o m i)
catchC pred f = capture handler
    where handler = \(Excep x k) ->
                    if pred x
                       then f x k handler
                       else callCC (\k' ->
                                    raise (Excep x
                                           (\v ->
                                            capture handler (k v) >>= k')))

-- This is the trapping version of catchC.
trap :: (Monad m, ResultType o) =>
        (o -> Bool) -> (o -> Cont o m i ->
                        (Excep o m i -> CPST (Excep o m i) m (Excep o m i)) ->
                        CPST (Excep o m i) m (Excep o m i)) ->
                       CPST (Excep o m i) m ()
trap pred f = install handler
    where handler = \(Excep x k) ->
                    if pred x
                       then f x k handler
                       else return (Excep x
                                (\x' -> capture handler (k x') >>= raise))

-- Like abort, ends in the middle.
end :: (Monad m, ResultType o) => CPST (Excep o m i) m i
end = raiseCC okValue

-- Needs to be added at the end of every do-sequence.  This returns OK
-- with a continuation that just regenerates itself.  It is needed to make
-- the types work out in the sequence.
done :: (Monad m, ResultType o) => CPST (Excep o m i) m (Excep o m i)
done = return (Excep okValue (\_ -> done))

-- Untypable, since i = Cont o i.
-- In other words, the continuation has to take itself as its argument.
--getcc :: CPST o m (Cont m o i)
--getcc = callCC (\k -> return k)
