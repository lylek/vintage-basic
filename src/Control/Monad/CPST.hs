{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}

-- | A continuation-passing style monad transformer, providing partial continuations.
-- Advantages of CPST over ContT include the definition of shift and reset in the
-- monad, and the rank-3 polymorphism for flexible contexts. The disadvantage is that
-- no other monad transformers can be composed on top of it, because it has too many
-- parameters.

module Control.Monad.CPST where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

-- | Definition of the CPST (continuation-passing style) monad.
newtype Monad m => CPST o m i =
      CPST { unCPST :: (i -> m o) -> m o }

instance (Monad m) => Functor (CPST o m) where
    fmap f (CPST m') = CPST (\k -> m' (k . f))

instance Monad m => Monad (CPST o m) where
      return x = CPST (\k -> k x)
      (CPST m') >>= f = CPST (\k -> m' (\x -> unCPST (f x) k))

-- The basic monad utilities: runCPST, lift, liftIO

-- | Runs code in the CPST monad.
runCPST :: Monad m => CPST o m o -> m o
runCPST (CPST m') = m' return

instance MonadTrans (CPST o) where
    lift m = CPST (\k -> do x <- m; k x)

instance (MonadIO m) => MonadIO (CPST o m) where
    liftIO = lift . liftIO

-- The core morphisms of CPST: shift, reset --

shift :: Monad m => ((forall a. i -> CPST a m o) -> CPST o m o) -> CPST o m i
-- ^ Captures a partial continuation out to the nearest enclosing 'reset'.
-- The rank-3 polymorphism permits the partial continuation to be used
-- in multiple differing type contexts.
shift f = CPST (\k -> runCPST (f (\x -> CPST (\k' -> k x >>= k'))))

reset :: Monad m => CPST i m i -> CPST o m i
reset m = CPST (\k -> do x <- runCPST m; k x)
-- ALTERNATELY: reset m = CPST (runCPST m >>=)

-- Other morphisms written in terms of shift & reset, without unwrapping
-- the monad.

abort :: Monad m => o -> CPST o m a
abort x = shift (\_ -> return x)
-- ALTERNATELY: abort x = CPST (\_ -> return x)

callCC :: Monad m => ((forall a. i -> CPST o m a) -> CPST o m i) -> CPST o m i
-- ^ Transforms a real continuation into a CPS-level continuation,
-- passes it into f.  The type is rank-3 polymorphic,
-- which permits the same continuation to be used in different
-- type contexts.  This is OK since invoking the continuation
-- disposes of the context.
callCC f = shift (\k -> (f (\x -> k x >>= abort)) >>= k)
-- ALTERNATELY: callCC f = CPST (\k -> unCPST (f (\x -> CPST (\_ -> k x))) k)

instance MonadReader r m => MonadReader r (CPST o m) where
    ask = lift ask
    local f (CPST m') =
        CPST (\k -> do r <- ask
                       local f (m' (\x -> local (const r) (k x))))

instance MonadState s m => MonadState s (CPST o m) where
    get = lift get
    put s = lift $ put s
