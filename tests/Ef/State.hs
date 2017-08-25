module Ef.State where

import Ef

type StateT s c a = Narrative (State s) c a

newtype State s k = State { runState :: s -> (s,k) }
  deriving Functor

{-# INLINE evalStateT #-}
evalStateT :: Monad c => Narrative (State s) c a -> s -> c a
evalStateT = foldn (\a _ -> return a) (\cf a -> cf >>= ($ a)) $
  \ssk s -> let (s',k) = runState ssk s in k s'

{-# INLINE get #-}
get :: Narrative (State s) c s
get = send (State (\s -> (s,s)))

{-# INLINE put #-}
put :: s -> Narrative (State s) c ()
put x = send (State (const (x,())))

{-# INLINE modify' #-}
modify' :: (s -> s) -> Narrative (State s) c ()
modify' f = send (State (\s -> let !s' = f s in (s',())))

{-# INLINE modify #-}
modify :: (s -> s) -> Narrative (State s) c ()
modify f = send (State (\s -> (f s,())))

