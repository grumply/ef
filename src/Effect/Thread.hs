module Effect.Thread
  ( Thread,thread
  , Threading, threads
  ) where

import Mop.Core
import Data.Queue

import Unsafe.Coerce
import Control.Monad

data Thread k
  = forall fs m a. Thread Int (Plan fs m a) k
  | Yield Int k
  | Stop Int
  | FreshScope (Int -> k)

{-# INLINE thread #-}
-- round-robin threading
-- use: thread $ \fork yield -> do { .. ; }
thread :: Has Thread fs m
     => ((forall b. Plan fs m b -> Plan fs m ()) -> Plan fs m () -> Plan fs m a)
     -> Plan fs m a
thread x = do
    scope <- self (FreshScope id)
    transform scope emptyQueue
      $ x (\p -> self (Thread scope (p >> self (Stop scope)) ()))
          (self (Yield scope ()))
  where
    transform scope q = go
      where
        go p = case p of
            Step syms bp -> case prj syms of
                Just x -> case x of
                    Thread i child k ->
                        if i == scope
                        then transform scope (enqueue (unsafeCoerce child) q) (bp k)
                        else Step syms (\b -> go (bp b))
                    Yield i k ->
                        if i == scope
                        then case dequeue q of
                               Nothing -> go (bp k)
                               Just (rest,nxt) ->
                                 transform scope (enqueue (unsafeCoerce (bp k)) rest) nxt
                        else Step syms (\b -> go (bp b))
                    Stop i ->
                      if i == scope
                      then case dequeue q of
                             Nothing -> Step syms (\b -> go (bp b))
                             Just (rest,nxt) -> transform scope rest nxt
                      else Step syms (\b -> go (bp b))
                Nothing -> Step syms (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> case dequeue q of
                Nothing -> Pure r
                Just (rest,nxt) -> transform scope rest (unsafeCoerce nxt)

instance Pair Threading Thread where
    pair p (Threading i k) (FreshScope ik) = p k (ik i)

data Threading k = Threading Int k

{-# INLINE threads #-}
threads :: Uses Threading gs m => Attribute Threading gs m
threads = Threading 0 $ \fs ->
  let Threading i k = (fs&)
      i' = succ i
  in i' `seq` pure (fs .= Threading i' k)
