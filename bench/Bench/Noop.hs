module Bench.Noop where

import Pure.Bench
import Pure.Test

import Ef
import qualified Ef.Noop as Noop
import qualified Ef.Interpreter as I

import Data.Functor.Identity

suite = scope "noop" $ tests
  [ noops ]

noops = do
  br0 <- nf "1 no-op" (runIdentity . Noop.noops . noopN) 1
  br1 <- nf "1 no-op; interpreted" (runIdentity . Noop.noopsI . noopNI) 1
  report br0 br1

  br0 <- nf "100 no-ops" (runIdentity . Noop.noops . noopN) 100
  br1 <- nf "100 no-ops; interpreted" (runIdentity . Noop.noopsI . noopNI) 100
  report br0 br1

{-# INLINE noopN #-}
noopN :: Monad m => Int -> Narrative Noop.Noop m ()
noopN n = go n
  where
    {-# INLINE go #-}
    go 0 = return ()
    go n = do
      send (Noop.Noop ())
      go (n - 1)

{-# INLINE noopNI #-}
noopNI :: Monad m => Int -> I.Interp ctx Noop.Noop m ()
noopNI n = go n
  where
    {-# INLINE go #-}
    go 0 = return ()
    go n = do
      I.send (Noop.Noop ())
      go (n - 1)

