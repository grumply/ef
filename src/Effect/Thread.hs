module Effect.Thread
  ( Thread,thread
  , Threading, threads
  ) where

import Mop.Core

import Unsafe.Coerce
import Control.Monad

data Thread k
  = forall fs m a. Thread (Plan fs m a) k
  | Yield k
  | Stop

{-# INLINE thread #-}
-- round-robin threading
-- use: thread $ \fork yield -> do { .. ; }
thread :: Has Thread fs m
     => ((forall b. Plan fs m b -> Plan fs m ()) -> Plan fs m () -> Plan fs m a)
     -> Plan fs m a
thread x = do
    transform emptyQueue
      $ x (\p -> self (Thread (p >> self Stop) ()))
          (self (Yield ()))
  where
    transform q p0 = go p0
      where
        go p = case p of
            Step syms bp -> case prj syms of
                Just x -> case x of
                    Thread child k ->
                        transform (enqueue (unsafeCoerce child) q) (bp k)
                    Yield k -> case dequeue q of
                        Nothing -> go (bp k)
                        Just (rest,nxt) ->
                            transform (enqueue (unsafeCoerce (bp k)) rest) nxt
                    Stop -> case dequeue q of
                        Nothing -> Step syms bp
                        Just (rest,nxt) -> transform rest nxt
                Nothing -> Step syms (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> case dequeue q of
                Nothing -> Pure r
                Just (rest,nxt) -> transform rest (unsafeCoerce nxt)

instance Pair Threading Thread where
    pair p (Threading k) Stop = p k undefined

data Threading k = Threading k

{-# INLINE threads #-}
threads :: Monad m => Attribute Threading gs m
threads = Threading return

-- amortized constant-time queue
data Queue = forall fs m a. Queue [Plan fs m a] [Plan fs m a]

{-# INLINE emptyQueue #-}
emptyQueue = Queue [] []

{-# INLINE newQueue #-}
newQueue stack = Queue stack []

{-# INLINE enqueue #-}
enqueue :: (forall fs m a. Plan fs m a) -> Queue -> Queue
enqueue a (Queue l r) = Queue l (a:r)

{-# INLINE dequeue #-}
dequeue :: Queue -> Maybe (Queue,Plan fs m a)
dequeue (Queue [] []) = Nothing
dequeue (Queue [] xs) =
    let stack = reverse xs
    in Just (Queue (tail stack) [],unsafeCoerce (head stack))
dequeue (Queue xs ys) = Just (Queue (tail xs) ys,unsafeCoerce (head xs))
