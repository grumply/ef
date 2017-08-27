{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module Bench.AltState where

import Trivial

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
  br1 <- nf "transformers" (runIdentity . mtl_test) 1
  br2 <- nf "ef" (runIdentity . ef_test) 1
  report br1 br2

{-# INLINE ef_test #-}
ef_test = interp ef_add_return

{-# INLINE mtl_test #-}
mtl_test = T.evalStateT mtl_add_return

{-# INLINE ef_add_return #-}
ef_add_return :: StateT Int Identity Int
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

type StateT s c a = Interp s (State s) c a

data State s k where
  Get :: (s -> k) -> State s k
  Put ::  s -> k  -> State s k
  deriving Functor

{-# INLINE eval #-}
eval :: Monad c => Narrative (State s) c a -> s -> c (s,a)
eval n = \s -> Ef.thread go n s
  where
    go (Get sk) s  = sk s s
    go (Put s k) _ =  k s

{-# INLINE interp #-}
interp :: Monad c => StateT s c a -> s -> c (s,a)
interp = I.thread (flip eval)

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
