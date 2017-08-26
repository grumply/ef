module Ef.State where

import Ef
import Control.Arrow

type StateT s c a = Narrative (State s) c a

newtype State s k = State { runState :: s -> (s,k) }
  deriving Functor

{-# INLINE [1] evalStateT #-}
evalStateT :: Monad c => Narrative (State s) c a -> s -> c (s,a)
evalStateT n = \s ->
  thread (\(State ssk) s -> let (s',k) = ssk s in k s') n s

{-# INLINE [1] get #-}
get :: Narrative (State s) c s
get = send (State (\s -> (s,s)))

{-# INLINE [1] put #-}
put :: s -> Narrative (State s) c ()
put s = send (State (\_ -> (s,())))

{-# INLINE [1] modify' #-}
modify' :: (s -> s) -> Narrative (State s) c ()
modify' f = send (State (\s -> let !s' = f s in (s',())))

{-# INLINE [1] modify #-}
modify :: (s -> s) -> Narrative (State s) c ()
modify f = send (State (\s -> (f s,())))

