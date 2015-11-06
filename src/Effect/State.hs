{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RankNTypes #-}
module Effect.State
    ( State, get, gets, put, puts, swap, modify, modify'
    , Store, store
    ) where

import Mop.Core

import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

data State st k = Modify (st -> st) (st -> k)

data Store st k = Store st (st -> k)

instance Pair (Store st) (State st) where
    pair p (Store st k) (Modify stst stk) =
        let st' = stst st
        in pair p (st,k st') stk

{-# INLINE get #-}
get :: Has (State st) fs m => Plan fs m st
get = self (Modify id id)

{-# INLINE gets #-}
gets :: Has (State st) fs m => (st -> a) -> Plan fs m a
gets f = self (Modify id f)

{-# INLINE put #-}
put :: Has (State st) fs m => st -> Plan fs m ()
put st = self (Modify (const st) (const ()))

{-# INLINE puts #-}
puts :: Has (State st) fs m => (a -> st) -> a -> Plan fs m ()
puts f a = self (Modify (const (f a)) (const ()))

{-# INLINE swap #-}
swap :: Has (State st) fs m => st -> Plan fs m st
swap st = self (Modify (const st) id)

{-# INLINE modify #-}
modify :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify f = self (Modify f (const ()))

{-# INLINE modify' #-}
modify' :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify' f = do
    st <- get
    put $! f st

{-# INLINE store #-}
store :: Uses (Store st) fs m => st -> Attribute (Store st) fs m
store st0 = Store st0 (\a fs -> pure $ fs .= store a)
