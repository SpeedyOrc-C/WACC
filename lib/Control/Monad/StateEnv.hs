module Control.Monad.StateEnv where

import Control.Monad.State

{- |
The superset of `MonadState`, with an extra capability that
reads a constant environment.
-}
class MonadState state (a env state) => MonadStateEnvironment env state a where
    {- |
    Similar to `get` in `MonadState`, but gets the read-only environment,
    not the writable state.
    -}
    see :: a env state env
    {- |
    Similar to `gets` in `MonadState`, but gets a specific field in the
    environment.
    -}
    sees :: (env -> envItem) -> a env state envItem
