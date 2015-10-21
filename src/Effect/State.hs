{-# LANGUAGE ExistentialQuantification #-}
module Effect.State
    ( State, get, gets, put, puts, swap, modify, modify'
    , Store, store
    , LocalState, LocalStore, localState, localStore
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
get = symbol (Modify id id)

gets :: Has (State st) fs m => (st -> a) -> Plan fs m a
gets f = symbol (Modify id f)

put :: Has (State st) fs m => st -> Plan fs m ()
put st = symbol (Modify (const st) (const ()))

puts :: Has (State st) fs m => (a -> st) -> a -> Plan fs m ()
puts f a = symbol (Modify (const (f a)) (const ()))

swap :: Has (State st) fs m => st -> Plan fs m st
swap st = symbol (Modify (const st) id)

modify :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify f = symbol (Modify f (const ()))

modify' :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify' f = do
    st <- get
    put $! f st

{-# INLINABLE store #-}
store :: Uses (Store st) fs m => st -> Attribute (Store st) fs m
store st0 = Store st0 (\a fs -> pure $ fs .= store a)

data LocalState k
    = FreshScope (Integer -> k)
    | Get Integer
    | forall st. Put Integer st

data LocalStore k = LocalStore (IORef Integer) k

{-# INLINE localStore #-}
localStore :: Uses LocalStore fs m => Attribute LocalStore fs m
localStore = LocalStore (unsafePerformIO $ newIORef 0) $ \fs ->
    let LocalStore i k = (fs&)
        x = unsafePerformIO (modifyIORef i succ)
    in x `seq` return fs

instance Pair LocalStore LocalState where
    pair p (LocalStore i k) (FreshScope ik) =
        let n = unsafePerformIO (readIORef i)
        in n `seq` p k (ik n)

{-# INLINE freshScope #-}
freshScope :: Has LocalState fs m => Plan fs m Integer
freshScope = symbol (FreshScope id)

{-# INLINE localState #-}
localState :: Has LocalState fs m
           => st
           -> (Plan fs m st -> (st -> Plan fs m ()) -> Plan fs m r)
           -> Plan fs m r
localState st f = do
    scope <- freshScope
    transform scope st $ f (symbol (Get scope)) (\st -> symbol (Put scope st))
    where
        transform scope st0 p0 = go st0 p0 where
            go st = go' where
                go' p = case p of
                    Step sym bp -> case prj sym of
                        Just x  -> case x of
                            Get i ->
                                if i == scope
                                then go' (bp (unsafeCoerce st))
                                else Step sym (\b -> go' (bp b))
                            Put i st' ->
                                if i == scope
                                then go (unsafeCoerce st')
                                        (bp (unsafeCoerce ()))
                                else Step sym (\b -> go' (bp b))
                            _ -> Step sym (\b -> go' (bp b))
                        Nothing -> Step sym (\b -> go' (bp b))
                    M m -> M (fmap go' m)
                    Pure r -> Pure r
