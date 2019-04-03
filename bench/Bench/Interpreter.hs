{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Bench.Interpreter where

import Pure.Bench
import Pure.Test

import Ef
import Ef.Interpreter as I
import qualified Ef.Noop as Noop
import qualified Bench.State as State

import Data.Functor.Identity

suite :: Test Sync ()
suite = scope "interpreter" $ tests
  [ noops
  ]

noops = scope "noop" $ do
  br0 <- nf "direct"      (runIdentity . Noop.noops . directNoopN) 100
  br1 <- nf "interpreter" (runIdentity . Noop.noopsI . interpretedNoopN ) 100
  report br0 br1

{-# INLINE directNoopN #-}
directNoopN :: Monad m => Int -> Narrative Noop.Noop m ()
directNoopN = go
  where
    {-# INLINE go #-}
    go 0 = return ()
    go n = do
      Ef.send (Noop.Noop ())
      go (n - 1)

{-# INLINE interpretedNoopN #-}
interpretedNoopN :: Monad m => Int -> Interp ctx Noop.Noop m ()
interpretedNoopN = go
  where
    {-# INLINE go #-}
    go 0 = return ()
    go n = do
      I.send (Noop.Noop ())
      go (n - 1)
