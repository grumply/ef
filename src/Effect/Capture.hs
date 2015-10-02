{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Effect.Capture where

import Mop

data Capture k' k
  = Checkpoint (k' -> k)
  | Recall k' k
data Reify k' k = Reify (k',k) (k' -> k)

checkpoint :: Has (Capture (Plan fs m a)) fs m
           => Plan fs m (Plan fs m a)
checkpoint = symbol (Checkpoint id)

recall :: Has (Capture (Plan fs m a)) fs m
       => Plan fs m a -> Plan fs m (Plan fs m a)
recall plan = symbol (Recall plan plan)

instance Pair (Reify k) (Capture k) where
  pair p (Reify kl _) (Checkpoint k) = pair p kl k
  pair p (Reify _ kr) (Recall k' k) = pair p kr (k',k)

reify :: Uses (Reify (Plan fs m a)) symbols m
      => Plan fs m a -> Instruction (Reify (Plan fs m a)) symbols m
reify f = Reify (f,pure) (instruction . reify)
