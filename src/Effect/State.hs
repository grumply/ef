module Effect.State
  ( State, get, put, modify, modify'
  , Store, store
  ) where

import Mop

data State st k = Modify (st -> st) (st -> k)

data Store st k = Store st (st -> k)

instance Pair (Store st) (State st) where
  pair p (Store st k) (Modify stst stk) =
    let st' = stst st
    in pair p (st',k st') stk

get :: Has (State st) fs m => Plan fs m st
get = symbol (Modify id id)

put :: Has (State st) fs m => st -> Plan fs m ()
put st = symbol (Modify (const st) (const ()))

modify :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify f = symbol (Modify f (const ()))

modify' :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify' f = do
  st <- get
  put $! f st

{-# INLINABLE store #-}
store :: Uses (Store st) fs m => st -> Instruction (Store st) fs m
store st0 = Store st0 (instruction . store)
