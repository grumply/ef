module Bench.Pipes where

import Trivial

import qualified Ef.Pipes as Ef
import qualified Pipes as Pipes

import Data.Functor.Identity

suite = scope "pipes" $ tests
  [ simple 10
  ]

simple n = scope ("simple " ++ show n) $ do
  br1 <- nf "ef"    (runIdentity . ef   ) n
  br2 <- nf "pipes" (runIdentity . pipes) n
  report br1 br2


ef, pipes :: Monad m => Int -> m Int
ef n = Ef.runEffect $ fmap (+1) $ produce n Ef.>-> consume
  where
    consume = do
      n <- Ef.await
      case n of
        1 -> return n
        _ -> consume

    produce 0 = return 0
    produce n = do
      Ef.yield n
      produce (n - 1)

pipes n = Pipes.runEffect $ fmap (+1) $ produce n Pipes.>-> consume
  where
    consume = do
      n <- Pipes.await
      case n of
        1 -> return n
        _ -> consume

    produce 0 = return 0
    produce n = do
      Pipes.yield n
      produce (n - 1)
