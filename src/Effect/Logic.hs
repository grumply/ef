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

-- nondeterministic choice with pruning

data Logic k
  = FreshScope (Integer -> k)
  | forall a. Choose Integer [a] (a -> k)
  | Cut Integer

data Nondet k = Nondet Integer k
nondet :: Uses Nondet fs m => Instruction Nondet fs m
nondet = Nondet 0 $ \fs ->
  let Nondet i k = view fs
  in instruction (Nondet (succ i) k) fs

freshScope :: Has Logic fs m => Plan fs m Integer
freshScope = symbol (FreshScope id)

-- a nondeterministic producer
-- use: logic $ \ch yield cut ->
--        ch [1..5] $ \x -> ch [2..5] $ \y -> ch [3..5] $ \z ->
--          when (a*a + b*b /= c*c) (yield (x,y,z))
--          cut
logic :: forall fs m r b.
         (Has Logic fs m,Has Weave fs m)
      => (   (forall a. [a] -> Plan fs m a)
          -> (b -> Plan fs m ())
          -> (forall d. Plan fs m d)
          -> Plan fs m r
         )
      -> Producer fs b m (Could r)
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
        try :: forall a. [a] -> (a -> Plan fs m r) -> Plan fs m (Could r) -> Plan fs m (Could r)
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
                Pure r -> Pure (Did r)
        go' p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Choose i as _ ->
                      if i == scope
                      then try as (unsafeCoerce bp) (Pure Didn't)
                      else Step sym (\b -> go' (bp b))
                    -- ignore cuts if no choices
                    Cut _ -> Step sym (\b -> go' (bp b))
                Nothing -> Step sym (\b -> go' (bp b))
            M m -> M (fmap go' m)
            Pure r -> Pure (Did r)

instance Pair Nondet Logic where
  pair p (Nondet i k) (FreshScope ik) = p k (ik i)
