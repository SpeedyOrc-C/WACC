module Control.Monad.StateEnv.Lazy where

class MonadStateEnvironment env state a where
    see :: a env state env
    sees :: (env -> envItem) -> a env state envItem
