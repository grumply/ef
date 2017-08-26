module Bench.State where

import Trivial

import Ef

import Ef.State

import qualified Control.Monad.Trans.State as T

import Data.Functor.Identity

suite :: Test Sync ()
suite = scope "state" $ tests
  [ state
  ]

state :: Test Sync ()
state = tests
  [ addReturn
  -- , fmapChain
  ]

addReturn :: Test Sync ()
addReturn = scope "add/return" $ do
  br1 <- nf "transformers" (runIdentity . T.evalStateT mtl_add_return) 1
  br2 <- nf "ef" (runIdentity . evalStateT ef_add_return) 1
  report br1 br2

fmapChain :: Test Sync ()
fmapChain = scope "fmap/fmap/fmap" $ do
  br1 <- nf "transformers" (runIdentity . T.evalStateT (fmap (+1) . fmap (+1) . fmap (+1) $ return (3 :: Int))) (1 :: Int)
  br2 <- nf "ef" (runIdentity . evalStateT (fmap (+1) . fmap (+1) . fmap (+1) $ return (3 :: Int))) (1 :: Int)
  report br1 br2

{-# INLINE ef_add_return #-}
ef_add_return :: Narrative (State Int) Identity Int
ef_add_return = go (10000 :: Int)
  where
    {-# INLINE go #-}
    go 0 = get
    go n = do
      x :: Int <- get
      y :: Int <- get
      put $! x + y
      go (n - 1)

{-# INLINE mtl_add_return #-}
mtl_add_return :: T.StateT Int Identity Int
mtl_add_return = go (10000 :: Int)
  where
    {-# INLINE go #-}
    go 0 = T.get
    go n = do
      x :: Int <- T.get
      y :: Int <- T.get
      T.put $! x + y
      go (n - 1)

