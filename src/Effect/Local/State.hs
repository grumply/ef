module Effect.Local.State
    ( State, state
    , Store, store
    , get, put, modify, modify'
    , gets, puts, swap
    ) where

import Mop

import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

data State k
    = FreshScope (Integer -> k)
    | Get Integer
    | forall a. Put Integer a

data Var fs m st = Var
    { get :: Plan fs m st
    , put :: st -> Plan fs m ()
    }

{-# INLINE modify #-}
modify :: Monad m => Var fs m st -> (st -> st) -> Plan fs m ()
modify Var{..} f = get >>= \a -> put (f a)

{-# INLINE modify' #-}
modify' :: Monad m => Var fs m st -> (st -> st) -> Plan fs m ()
modify' Var{..} f = get >>= \a -> put $! f a

{-# INLINE gets #-}
gets :: Monad m => Var fs m st -> (st -> a) -> Plan fs m a
gets Var{..} f = f <$> get

{-# INLINE puts #-}
puts :: Monad m => Var fs m st -> (a -> st) -> a -> Plan fs m ()
puts Var{..} f new = put (f new)

{-# INLINE swap #-}
swap :: Monad m => Var fs m st -> st -> Plan fs m st
swap Var{..} st_new = get >>= \st_old -> put st_new >> return st_old

{-# INLINE state #-}
state :: forall fs m st r. Has State fs m
      => st -> (Var fs m st -> Plan fs m r) -> Plan fs m (st,r)
state st f = do
    scope <- self (FreshScope id)
    transform scope st $ f Var
        { get = self (Get scope)
        , put = \st -> self (Put scope st)
        }
  where
    transform scope = go where
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
                Pure r -> Pure (st,r)

data Store k = Store (IORef Integer) k

{-# INLINE store #-}
store :: Uses Store fs m => Attribute Store fs m
store = Store (unsafePerformIO $ newIORef 0) $ \fs ->
    let Store i k = (fs&)
        x = unsafePerformIO (modifyIORef i succ)
    in x `seq` return fs

varMisuse :: String -> a
varMisuse method = error $
  "Var misuse: " ++ method ++ " used outside of its 'state' block. \
  \Do not return a Var or its internal fields from its instantiation block."

instance Pair Store State where
    pair p (Store i k) (FreshScope ik) =
        let n = unsafePerformIO (readIORef i)
        in n `seq` p k (ik n)
    pair p _ (Get _) = varMisuse "Effect.Local.State.Lazy.Get"
    pair p _ (Put _ _) = varMisuse "Effect.Local.State.Lazy.Put"
