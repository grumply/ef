{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module Bench.AltState where

import Pure.Bench
import Pure.Test

import Ef

import Ef.Interpreter as I

import qualified Control.Monad.Trans.State as T

import Data.Functor.Identity

suite :: Test Sync ()
suite = scope "altstate" $ tests
  [ Bench.AltState.state
  ]

state :: Test Sync ()
state = tests
  [ addReturn
  -- , fmapChain
  ]

addReturn :: Test Sync ()
addReturn = scope "add/return" $ do
  br1 <- scope "transformers" $ nf (runIdentity . mtl_test) 1
  br2 <- scope "ef" $ nf (runIdentity . ef_test) 1
  report br1 br2

ef_test = eval ef_add_return

mtl_test = T.evalStateT mtl_add_return

ef_add_return :: StateT Int Identity ()
ef_add_return = forM_ [1..1000 :: Int] $ \_ -> do
  x :: Int <- get
  y <- get
  put $! x + y

mtl_add_return :: T.StateT Int Identity ()
mtl_add_return = forM_ [1..1000 :: Int] $ \_ -> do
  x :: Int <- T.get
  y <- T.get
  T.put $! x + y

type StateT s c a = Interp s (State s) c a

data State s k where
  Get :: (s -> k) -> State s k
  Put ::  s -> k  -> State s k
  deriving Functor

{-# INLINE eval #-}
eval :: Monad c => StateT s c a -> s -> c (s,a)
eval = I.thread (Ef.thread go)
  where
    go (Get sk) s  = sk s s
    go (Put s k) _ =  k s

{-# INLINE get #-}
get :: StateT s c s
get = I.send (Get id)

{-# INLINE put #-}
put :: s -> StateT s c ()
put s = I.send (Put s ())

-- {-# INLINE modify' #-}
-- modify' :: (s -> s) -> StateT s c ()
-- modify' f = I.send (State (\s -> let !s' = f s in (s',())))

-- {-# INLINE modify #-}
-- modify :: (s -> s) -> StateT s c ()
-- modify f = I.send (State (\s -> (f s,())))
