{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Effect.State where

import Mop

data State st k
  = Get (st -> k)
  | Put st k

data Store st k = Store !st k (st -> k)

instance Pair (Store st) (State st) where
  {-# INLINE pair #-}
  pair p (Store st k _  ) (Get stk ) = pair p (st,k) stk
  pair p (Store _  _ stk) (Put st k) = pair p stk (st,k)

{-# INLINABLE get #-}
get :: Has (State st) fs m => Plan fs m st
get = symbol (Get id)

{-# INLINABLE put #-}
put :: Has (State st) fs m => st -> Plan fs m ()
put st = symbol (Put st ())

{-# INLINABLE modify #-}
modify :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify f = do
  st <- get
  put $ f st

{-# INLINABLE store #-}
store :: Uses (Store st) fs m => st -> Instruction (Store st) fs m
store st = Store st pure (instruction . store)
