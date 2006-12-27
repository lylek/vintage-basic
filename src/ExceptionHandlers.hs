-- ExceptionHandlers.hs
-- Exception handlers for CPST, both 'catching' and 'trapping'
-- Lyle Kopnicky

module ExceptionHandlers where

import Control.Monad
import CPST

-- The inward (catching) exception handlers --

capture :: Monad m => (o1 -> CPST o m i) -> CPST o1 m o1 -> CPST o m i
capture f m = reset m >>= f

tack :: Monad m => CPST o m i -> CPST o1 m o1 -> CPST o m i
tack m = capture (\_ -> m)

handle :: Monad m => (o1 -> Bool) -> (o1 -> CPST o m o1) -> CPST o1 m o1
       -> CPST o m o1
handle pred f = capture (\x -> if pred x then f x else return x)

-- The outward (trapping) exception handlers --

captureAtEnd :: Monad m => (o -> CPST o m o) -> CPST o m ()
--captureAtEnd f = CPST (\k -> do x <- k (); runCPST (f x))
--captureAtEnd f = callCC (\k -> reset (k ()) >>= f >>= abort)
captureAtEnd f = shift (\k -> reset (k ()) >>= f)

tackAtEnd :: Monad m => CPST o m o -> CPST o m ()
--tackAtEnd m = CPST (\k -> do k (); runCPST m)
tackAtEnd m = captureAtEnd (\_ -> m)

handleAtEnd :: Monad m => (o -> Bool) -> (o -> CPST o m o) -> CPST o m ()
handleAtEnd pred f = captureAtEnd (\x -> if pred x then f x else return x)

-- Some convenient aliases

raise :: Monad m => o -> CPST o m a
raise = abort

install :: Monad m => (o -> CPST o m o) -> CPST o m ()
install = captureAtEnd
