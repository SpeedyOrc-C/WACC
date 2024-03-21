module Control.Monad.Trans.StateEnv.Lazy where

import           Control.Monad.State (MonadState(..))
import           Control.Monad.StateEnv.Lazy
import           Control.Arrow

{- |
Enhanced State transformer with an extra read only field.

The computation reads the environment and a writable state,
and gives a result with a modified state.
-}
newtype State env state a = State (env -> state -> (a, state))

instance Functor (State env state) where
    fmap :: (a -> b) -> State env state a -> State env state b
    fmap f (State a) = State (\e s -> first f (a e s))

instance Applicative (State env state) where
    pure :: a -> State env state a
    pure r = State (\_ s -> (r, s))

    (<*>) :: State env state (a -> b) -> State env state a -> State env state b
    State a <*> State a' = State $ \e s -> let
        (f, s') = a e s
        (x, s'') = a' e s'
        in
        (f x, s'')

instance Monad (State env state) where
    return :: a -> State env state a
    return = pure

    (>>=) :: State env state a -> (a -> State env state b) -> State env state b
    State a >>= f = State $ \e s -> let
        (x, s') = a e s
        State a' = f x
        in
        a' e s'

instance MonadState state (State env state) where
    get :: State env state state
    get = State $ \_ s -> (s, s)

    put :: state -> State env state ()
    put s = State $ \_ _ -> ((), s)

instance MonadStateEnvironment env state State where
    see :: State env state env
    see = State (,)

    sees :: (env -> a) -> State env state a
    sees item = State $ \e s -> (item e, s)

runState :: State env state a -> env -> state -> (a, state)
runState (State t) = t

evalState :: State env state a -> env -> state -> a
evalState (State t) e s = fst $ t e s

execState :: State env state a -> env -> state -> state
execState (State t) e s = snd $ t e s
