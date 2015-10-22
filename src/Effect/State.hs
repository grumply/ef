{-# LANGUAGE ExistentialQuantification #-}
module Effect.State
    ( State, get, gets, put, puts, swap, modify, modify'
    , Store, store
    ) where

import Mop

import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

data State st k = Modify (st -> st) (st -> k)

data Store st k = Store st (st -> k)

instance Pair (Store st) (State st) where
    pair p (Store st k) (Modify stst stk) =
        let st' = stst st
        in pair p (st,k st') stk

get :: Has (State st) fs m => Plan fs m st
get = self (Modify id id)

gets :: Has (State st) fs m => (st -> a) -> Plan fs m a
gets f = self (Modify id f)

put :: Has (State st) fs m => st -> Plan fs m ()
put st = self (Modify (const st) (const ()))

puts :: Has (State st) fs m => (a -> st) -> a -> Plan fs m ()
puts f a = self (Modify (const (f a)) (const ()))

swap :: Has (State st) fs m => st -> Plan fs m st
swap st = self (Modify (const st) id)

modify :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify f = self (Modify f (const ()))

modify' :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify' f = do
    st <- get
    put $! f st

{-# INLINABLE store #-}
store :: Uses (Store st) fs m => st -> Attribute (Store st) fs m
store st0 = Store st0 (\a fs -> pure $ fs .= store a)
