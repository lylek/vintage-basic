-- CPSTInstances.hs
-- Instances of CPST for other monad classes
-- Lyle Kopnicky
-- last updated 2004-09-17

module CPSTInstances where

import CPST
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State

instance MonadReader r m => MonadReader r (CPST o m) where
    ask = lift ask
    local f (CPST m') =
        CPST (\k -> do r <- ask
                       local f (m' (\x -> local (const r) (k x))))

instance MonadState s m => MonadState s (CPST o m) where
    get = lift get
    put s = lift $ put s
