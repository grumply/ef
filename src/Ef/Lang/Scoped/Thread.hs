{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Scoped.Thread
  ( Threading, threads
  , Threadable, threader
  , Thread(..)
  ) where

import Ef.Core
import Ef.Data.Queue

import Unsafe.Coerce

-- | Symbols

data Threading k
  = forall fs m a. Fork Int (Pattern fs m a) k
  | Yield Int k
  | Stop Int
  | FreshScope (Int -> k)

-- | Symbol Module

data Thread fs m = Thread
  { fork :: Pattern fs m () -> Pattern fs m ()
  , yield :: Pattern fs m ()
  }

-- | Attribute

data Threadable k = Threadable Int k

-- | Attribute Construct

{-# INLINE threader #-}
threader :: Uses Threadable gs m => Attribute Threadable gs m
threader = Threadable 0 $ \fs ->
  let Threadable i k = view fs
      i' = succ i
  in i' `seq` pure (fs .= Threadable i' k)

-- | Attribute/Symbol Symmetry

instance Symmetry Threadable Threading where
    symmetry use (Threadable i k) (FreshScope ik) = use k (ik i)

-- | Local Scoping Construct + Substitution

{-# INLINE threads #-}
-- round-robin threading
-- use: thread $ \fork yield -> do { .. ; }
threads :: Is Threading fs m
     => (Thread fs m -> Pattern fs m a)
     -> Pattern fs m a
threads f = do
    scope <- self (FreshScope id)
    transform scope emptyQueue $ f Thread
      { fork = \p -> self (Fork scope (p >> self (Stop scope)) ())
      , yield = self (Yield scope ())
      }
  where
    transform scope q = go
      where
        go p = case p of
            Step syms bp -> case prj syms of
                Just x -> case x of
                    Fork i child k ->
                        if i == scope
                        then transform scope (enqueue (unsafeCoerce child) q) (bp k)
                        else Step syms (\b -> go (bp b))
                    Yield i k ->
                        if i == scope
                        then case dequeue q of
                               Nothing -> go (bp k)
                               Just (rest,nxt) ->
                                 transform scope (enqueue (unsafeCoerce (bp k)) rest) nxt
                        else Step syms (\b -> go (bp b))
                    Stop i ->
                      if i == scope
                      then case dequeue q of
                             Nothing -> Step syms (\b -> go (bp b))
                             Just (rest,nxt) -> transform scope rest nxt
                      else Step syms (\b -> go (bp b))
                Nothing -> Step syms (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> case dequeue q of
                Nothing -> Pure r
                Just (rest,nxt) -> transform scope rest (unsafeCoerce nxt)
