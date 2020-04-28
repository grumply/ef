{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
module Bench.State where

import Pure.Bench
import Pure.Test

import Ef

import Ef.Interpreter as I

import qualified Control.Monad.Trans.State as T

import Data.Functor.Identity

import Control.Concurrent

suite :: Test Sync ()
suite = scope "state" $ tests
  [ Bench.State.state
  ]

state :: Test Sync ()
state = tests
  [ addReturn
  , fmapChain
  ]

{-# INLINE addReturn #-}
addReturn :: Test Sync ()
addReturn = scope "add/return" $ do
  let (_,a) = runIdentity (interp ef_add_return 1)
      b = runIdentity (T.evalStateT mtl_add_return 1)
  expect (a == b)
  scope "transformers" $ whnf (runIdentity . T.evalStateT mtl_add_return) 1 >>= summary
  scope "ef" $ whnf (runIdentity . interp ef_add_return) 1 >>= summary

{-# INLINE fmapChain #-}
fmapChain :: Test Sync ()
fmapChain = scope "fmap/fmap/fmap" $ do
  scope "transformers" $ whnf (runIdentity . T.evalStateT mtl_fmap) (1 :: Int) >>= summary
  scope "ef" $ whnf (runIdentity . interp ef_fmap) (1 :: Int) >>= summary

{-# INLINE mtl_fmap #-}
mtl_fmap :: Monad m => T.StateT Int m Int
mtl_fmap = fmap (+1) . fmap (+1) . fmap (+1) $ T.get

{-# INLINE ef_fmap #-}
ef_fmap :: Monad m => StateT Int m Int
ef_fmap = fmap (+1) . fmap (+1) . fmap (+1) $ get

{-# INLINE ef_add_return #-}
ef_add_return :: Monad m => StateT Int m Int
ef_add_return = do
  w :: Int <- get
  x :: Int <- get
  y :: Int <- get
  z :: Int <- get
  put $! w + x + y + z
  get

{-# INLINE mtl_add_return #-}
mtl_add_return :: Monad m => T.StateT Int m Int
mtl_add_return = do
  w :: Int <- T.get
  x :: Int <- T.get
  y :: Int <- T.get
  z :: Int <- T.get
  T.put $! w + x + y + z
  T.get

type StateT s c a = Interp s (State s) c a

newtype State s k = State { runState :: s -> (s,k) }
  deriving Functor

{-# INLINE eval #-}
eval :: Monad c => Narrative (State s) c a -> s -> c (s,a)
eval n = \s ->
  Ef.thread (\(State ssk) s -> let (s',k) = ssk s in k s') n s

{-# INLINE interp #-}
interp :: Monad c => StateT s c a -> s -> c (s,a)
interp = I.thread eval

{-# INLINE get #-}
get :: StateT s c s
get = I.send (State (\s -> (s,s)))

{-# INLINE put #-}
put :: s -> StateT s c ()
put s = I.send (State (\_ -> (s,())))

{-# INLINE modify' #-}
modify' :: (s -> s) -> StateT s c ()
modify' f = I.send (State (\s -> let !s' = f s in (s',())))

{-# INLINE modify #-}
modify :: (s -> s) -> StateT s c ()
modify f = I.send (State (\s -> (f s,())))
