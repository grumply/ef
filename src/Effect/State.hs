{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Effect.State
  ( State(..), get, gets, put, puts, swap, modify, modify'
  , Store(..), store
  ) where

import Mop

data State st k = Modify (st -> st) (st -> k)

data Store st k = Store st (st -> k)

instance Pair (Store st) (State st) where
  pair p (Store st k) (Modify stst stk) = pair p (st,k (stst st)) stk

get :: Has (State st) fs m => Plan fs m st
get = symbol (Modify id id)

gets :: Has (State st) fs m => (st -> x)-> Plan fs m x
gets f = symbol (Modify id f)

put :: Has (State st) fs m => st -> Plan fs m ()
put st = symbol (Modify (const st) (const ()))

puts :: Has (State st) fs m => x -> (x -> st) -> Plan fs m ()
puts x f = symbol (Modify (const (f x)) (const ()))

swap :: Has (State st) fs m => st -> Plan fs m st
swap st = symbol (Modify (const st) id)

modify :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify f = symbol (Modify f (const ()))

modify' :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify' f = do
  st <- get
  put $! f st

{-# INLINABLE store #-}
store :: Uses (Store st) fs m => st -> Instruction (Store st) fs m
store st0 = Store st0 (instruction . store)
