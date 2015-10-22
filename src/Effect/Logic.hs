{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Effect.Logic
    ( Logic, logic
    , Nondet, nondet
    , module Effect.Weave
    ) where

import Mop
import Effect.Weave
import Unsafe.Coerce
import Data.IORef
import System.IO.Unsafe
import Debug.Trace

-- nondeterministic choice with pruning

data Logic k
    = FreshScope (Integer -> k)
    | forall a. Choose Integer [a] (a -> k)
    | Cut Integer

data Nondet k = Nondet (IORef Integer) k

{-# INLINE nondet #-}
nondet :: Uses Nondet fs m => Attribute Nondet fs m
nondet = Nondet (unsafePerformIO (newIORef 0)) $ \fs ->
    let Nondet i k = (fs&)
        next = unsafePerformIO (modifyIORef i succ)
    in next `seq` return fs


{-# INLINABLE freshScope #-}
freshScope :: Has Logic fs m => Plan fs m Integer
freshScope = self (FreshScope id)

-- a nondeterministic producer
-- use: pythag n = logic $ \ch yield cut ->
--        ch [1..n] >>= \z -> ch [1..z] >>= \x -> ch [x..z] >>= \y -> do
--          when (x*x + y*y /= z*z) (yield (x,y,z))
--          cut
{-# INLINE logic #-}
logic :: forall fs m r b.
         (Has Logic fs m,Has Weave fs m)
      => (   (forall a. [a] -> Plan fs m a)
          -> (b -> Plan fs m ())
          -> (forall d. Plan fs m d)
          -> Plan fs m ()
         )
      -> Producer fs b m ()
logic l =
  producer $ \yield -> do
    scope <- freshScope
    go scope
      $ l (\as -> self (Choose scope as id))
          yield
          (self (Cut scope))
  where
    go scope p0 = go' p0
      where
        try :: forall a. [a] -> (a -> Plan fs m ()) -> Plan fs m () -> Plan fs m ()
        try [] bp or = or
        try (a:as) bp or = try' (bp a)
          where
            try' p = case p of
                Step sym bp' -> case prj sym of
                    Just x -> case x of
                        Choose i as' _ ->
                            if i == scope
                            then try as' (unsafeCoerce bp') (try as bp or)
                            else Step sym (\b -> try' (bp' b))
                        Cut i ->
                            if i == scope
                            then try as bp or
                            else Step sym (\b -> try' (bp' b))
                    Nothing -> Step sym (\b -> try' (bp' b))
                M m -> M (fmap try' m)
                Pure r -> try as bp or
        go' p = case p of
            Step sym bp -> case prj sym of
                Just x -> case x of
                    Choose i as _ ->
                        if i == scope
                        then try as (unsafeCoerce bp) (return ())
                        else Step sym (\b -> go' (bp b))
                    -- ignore cuts if no choices
                    Cut _ -> Step sym (\b -> go' (bp b))
                Nothing -> Step sym (\b -> go' (bp b))
            M m -> M (fmap go' m)
            Pure r -> Pure r

instance Pair Nondet Logic where
    pair p (Nondet i k) (FreshScope ik) =
        let n = (unsafePerformIO $ readIORef i)
        in n `seq` p k (ik n)
    pair _ _ _ = error "Logic primitive escaped its scope:\n\
                       \\tAttempting to reuse control flow\
                       \ primitives outside of their scope\
                       \ is unsupported."
