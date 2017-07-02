-- | Exception handlers for the CPST monad, both \'catching\' (within an expression)
-- and \'trapping\' (from the handler's continuation).

module Control.Monad.CPST.ExceptionHandlers where

import Control.Monad.CPST

-- The inward (catching) exception handlers --

-- | Captures an exception that occurs within the specified computation.
-- Like @try-catch@ in Java, but without the filtering of exceptions.
capture :: Monad m
    => (o1 -> CPST o m i) -- ^ the exception handler
    -> CPST o1 m o1       -- ^ the computation within which to capture exceptions
    -> CPST o m i
capture f m = reset m >>= f

-- | Forces a computation to occur after another one, even if the first one is
-- aborted. Like @try-finally@ in Java.
tack :: Monad m
    => CPST o m i   -- ^ the computation to tack on (@finally@ clause)
    -> CPST o1 m o1 -- ^ the computation to wrap (@try@ clause)
    -> CPST o m i
tack m = capture (\_ -> m)

-- | Catches exceptions in a computation that satisfy a predicate, and applies
-- a function to them. If the exception does not match the predicate, it
-- rethrows the exception. This is like @try-catch@ in Java.
handle :: Monad m
    => (o -> Bool)       -- ^ the condition for matching the exception
    -> (o -> CPST o m o) -- ^ the exception handler
    -> CPST o m o        -- ^ the computation within which to catch exceptions
    -> CPST o m o
handle t f = capture (\x -> if t x then f x else abort x)

-- The outward (trapping) exception handlers --

-- | Captures an exception that occurs in the continuation of this statement.
-- Like a @trap@ statement in Bourne shell or Windows PowerShell, but without
-- discriminating over the exception type.
captureAtEnd :: Monad m
    => (o -> CPST o m o) -- ^ the exception handler
    -> CPST o m ()
captureAtEnd f = shift (\k -> reset (k ()) >>= f)
-- ALTERNATELY: captureAtEnd f = CPST (\k -> do x <- k (); runCPST (f x))
-- ALTERNATELY: captureAtEnd f = callCC (\k -> reset (k ()) >>= f >>= abort)

-- | Forces a computation to occur at the end of execution. Like an @END@
-- block in Perl or @atexit()@ in C.
tackAtEnd :: Monad m
    => CPST o m o  -- ^ the computation to tack at the end of execution
    -> CPST o m ()
tackAtEnd m = captureAtEnd (\_ -> m)
-- ALTERNATELY: tackAtEnd m = CPST (\k -> do k (); runCPST m)

-- | Handles an exception that occurs in the continuation of this statement.
-- Like a @trap@ statement in Bourne shell or Windows PowerShell.
-- The exception is only caught if it matches a predicate. Otherwise, it is
-- passed on to the next outer handler.
handleAtEnd :: Monad m
    => (o -> Bool)       -- ^ the condition for matching the exception
    -> (o -> CPST o m o) -- ^ the exception handler
    -> CPST o m ()
handleAtEnd t f = captureAtEnd (\x -> if t x then f x else return x)

-- Some convenient aliases

-- | An alias for 'abort'.
raise :: Monad m => o -> CPST o m a
raise = abort

-- | An alias for 'captureAtEnd'.
install :: Monad m => (o -> CPST o m o) -> CPST o m ()
install = captureAtEnd
