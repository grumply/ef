module Bench.Pipes.Chain where

import Trivial

import qualified Ef.Pipes as Ef
import qualified Pipes as Pipes
import qualified Pipes.Prelude as Pipes

import Data.Functor.Identity

suite = scope "chain" $ tests
  [ simple 10000
  ]

simple n = scope ("simple " ++ show n) $ do
  br1 <- nfio "ef"    (ef    n)
  br2 <- nfio "pipes" (pipes n)
  report br1 br2

{-# INLINE [1] ef_map #-}
ef_map :: Monad m => (a -> b) -> Ef.Pipe a b m r
ef_map f = go
  where
    go = do
      x <- Ef.await
      Ef.yield (f x)
      go
{-# RULES
    "p >-> ef_map f" forall p f . p Ef.>-> ef_map f = Ef.for p (\a -> Ef.yield (f a))

  ; "ef_map f >-> p" forall p f . ef_map f Ef.>-> p = (do
        a <- Ef.await
        return (f a) ) Ef.>~ p
  #-}

{-# INLINE [1] ef_filter #-}
ef_filter :: Monad m => (a -> Bool) -> Ef.Pipe a a m r
ef_filter f = go
  where
    go = do
      x <- Ef.await
      when (f x) (Ef.yield x)
      go
{-# RULES
    "p >-> ef_filter predicate" forall p predicate.
        p Ef.>-> ef_filter predicate = Ef.for p (\a -> when (predicate a) (Ef.yield a))
  #-}

ef, pipes :: (Monad m) => Int -> m ()
ef n = Ef.runEffect $ Ef.for (Ef.each [1..n] Ef.>-> ef_map (+1) Ef.>-> ef_filter even) Ef.discard

pipes n = Pipes.runEffect $ Pipes.for (Pipes.each [1..n] Pipes.>-> Pipes.map (+1) Pipes.>-> Pipes.filter even) Pipes.discard

