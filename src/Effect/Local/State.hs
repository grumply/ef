{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Local.State
    ( State, state
    , Store, store
    , Var(..)
    ) where

import Mop.Core

import Unsafe.Coerce

data Eagerness = Strict | Lazy deriving Eq

data State k
    = FreshScope (Int -> k)
    | forall a. Modify Int Eagerness (a -> a) (a -> k)

data Var fs m st = Var
  { modify :: (st -> st) -> Plan fs m ()
  , modify' :: (st -> st) -> Plan fs m ()
  , get :: Plan fs m st
  , gets :: forall a. (st -> a) -> Plan fs m a
  , put :: st -> Plan fs m ()
  , puts :: forall a. (a -> st) -> a -> Plan fs m ()
  , swap :: st -> Plan fs m st
  }

{-# INLINE state #-}
state :: forall fs m st r. Has State fs m
      => st -> (Var fs m st -> Plan fs m r) -> Plan fs m (st,r)
state st f = do
    scope <- self (FreshScope id)
    transform scope st $ f Var
      { modify = \f -> self (Modify scope Lazy f (const ()))
      , modify' = \f -> self (Modify scope Strict f (const ()))
      , get = self (Modify scope Lazy id id)
      , gets = \f -> self (Modify scope Lazy id f)
      , put = \st -> self (Modify scope Lazy (const st) (const ()))
      , puts = \f a -> self (Modify scope Lazy (const (f a)) (const ()))
      , swap = \a -> self (Modify scope Lazy (const a) id)
      }
  where
    transform scope = go where
        go st = go' where
            go' p = case p of
                Step sym bp -> case prj sym of
                    Just x  -> case x of
                        Modify i sl f g ->
                            if i == scope
                            then
                              if sl == Strict
                              then let st' = unsafeCoerce f st
                                    in st' `seq` go st' (bp (unsafeCoerce g st))
                              else go (unsafeCoerce f st) (bp (unsafeCoerce g st))
                            else Step sym (\b -> go' (bp b))
                        _ -> Step sym (\b -> go' (bp b))
                    Nothing -> Step sym (\b -> go' (bp b))
                M m -> M (fmap go' m)
                Pure r -> Pure (st,r)

data Store k = Store Int k

{-# INLINE store #-}
store :: Uses Store fs m => Attribute Store fs m
store = Store 0 $ \fs ->
    let Store i k = (fs&)
        i' = succ i
    in i' `seq` pure (fs .= Store i' k)

varMisuse :: String -> a
varMisuse method = error $
  "Var misuse: " ++ method ++ " used outside of its 'state' block. \
  \Do not return a Var or its internal fields from its instantiation block."

instance Pair Store State where
    pair p (Store i k) (FreshScope ik) = p k (ik i)
