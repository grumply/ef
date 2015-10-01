{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Effect.State
  ( State, get, put, modify, modify'
  , Store, store
  ) where

import Mop

data State st k
  = Get (st -> k)
  | Put st k
  | Modify (st -> st) k

data Store st k = Store st (st -> st) k (st -> k) ((st -> st) -> k)

instance Pair (Store st) (State st) where
  pair p (Store st f k _   _   ) (Get    stk  ) = pair p (f st,k) stk
  pair p (Store _  _ _ stk _   ) (Put    st  k) = pair p stk    (st,k)
  pair p (Store _  _ _ _   fstk) (Modify f   k) = pair p fstk   (f,k)

get :: Has (State st) fs m => Plan fs m st
get = symbol (Get id)

put :: Has (State st) fs m => st -> Plan fs m ()
put st = symbol (Put st ())

modify :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify f = symbol (Modify f ())

modify' :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify' f = do
  st <- get
  put $! f st

store' :: forall st fs m. Uses (Store st) fs m => st -> (st -> st) -> Instruction (Store st) fs m
store' st f = Store st f pure (instruction . flip store' id) (\f' is ->
  let Store (new :: st) f0 g p m = view is
  in instruction (Store new (f' . f0) g p m) is
  )

store :: Uses (Store st) fs m => st -> Instruction (Store st) fs m
store x = store' x id
