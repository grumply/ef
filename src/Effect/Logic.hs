{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Effect.Logic
  ( Logic, logic
  , Nondet, nondet
  , module Data.Could
  , module Effect.Weave
  ) where

import Mop
import Effect.Weave
import Data.Could
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
nondet :: Uses Nondet fs m => Instruction Nondet fs m
nondet = Nondet (unsafePerformIO (newIORef 0)) $ \fs ->
  let Nondet i k = view fs
      next = unsafePerformIO (modifyIORef i succ)
  in next `seq` return fs


{-# INLINABLE freshScope #-}
freshScope :: Has Logic fs m => PlanT fs m Integer
freshScope = symbol (FreshScope id)

-- a nondeterministic producer
-- use: pythag n = logic $ \ch yield cut ->
--        ch [1..n] >>= \z -> ch [1..z] >>= \x -> ch [x..z] >>= \y -> do
--          when (x*x + y*y /= z*z) (yield (x,y,z))
--          cut
{-# INLINE logic #-}
logic :: forall fs m r b.
         (Has Logic fs m,Has Weave fs m)
      => (   (forall a. [a] -> PlanT fs m a)
          -> (b -> PlanT fs m ())
          -> (forall d. PlanT fs m d)
          -> PlanT fs m ()
         )
      -> Producer fs b m ()
logic l =
  producer $ \yield -> do
    scope <- freshScope
    go scope
      $ l (\as -> symbol (Choose scope as id))
          yield
          (symbol (Cut scope))
  where
    go scope p0 = go' p0
      where
        try :: forall a. [a] -> (a -> PlanT fs m ()) -> PlanT fs m () -> PlanT fs m ()
        try [] bp or = or
        try (a:as) bp or = try' (bp a)
          where
            try' p =
              case p of
                Step sym bp' ->
                  case prj sym of
                    Just x ->
                      case x of
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
        go' p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Choose i as _ ->
                      if i == scope
                      then try as (unsafeCoerce bp) (Pure ())
                      else Step sym (\b -> go' (bp b))
                    -- ignore cuts if no choices
                    Cut _ -> Step sym (\b -> go' (bp b))
                Nothing -> Step sym (\b -> go' (bp b))
            M m -> M (fmap go' m)
            Pure r -> Pure r

instance Pair Nondet Logic where
  pair p (Nondet i k) (FreshScope ik) =
    let n = (unsafePerformIO $ readIORef i)
    in trace (show n) $ p k (ik n)
  pair _ _ _ = error "Logic primitive escaped its scope:\n\
                     \\tAttempting to reuse control flow\
                     \ primitives outside of their scope\
                     \ is unsupported."
