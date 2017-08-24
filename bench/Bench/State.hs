{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Bench.State where

import Trivial
import Ef hiding (improve)

import qualified Control.Monad.Trans.State as St
import Data.Functor.Identity

stateSuite :: Test Sync ()
stateSuite = tests [
--   justReturn,
  addReturn
  ]

{-# INLINE ef_add_return #-}
ef_add_return :: Narrative (State Int) Identity Int
ef_add_return = go (10000 :: Int)
  where
    go 0 = get
    go n = do
      x :: Int <- get
      y :: Int <- get
      put $ x + y
      go (n - 1)

mtl_add_return :: St.StateT Int Identity Int
mtl_add_return = go (10000 :: Int)
  where
    go 0 = St.get
    go n = do
      x :: Int <- St.get
      y :: Int <- St.get
      St.put $ x + y
      go (n - 1)

newtype State s k = State (s -> (s,k))
  deriving Functor

-- instance Delta (State s) (State s) where
--   delta eval (State s k _ ) (Get f  ) = eval k (f s)
--   delta eval (State _ _ sk) (Put s k) = eval (sk s) k

{-# INLINE evalState #-}
evalState :: Monad c => Narrative (State s) c a -> s -> c a
evalState = foldn (\a _ -> return a) (\cf a -> cf >>= ($ a)) go
  where
    {-# INLINE go #-}
    go (State ssk) !s = let (!s',k) = ssk s in k s'

{-# INLINE get #-}
get :: Narrative (State s) c s
get = buildn $ \ret _ d -> d (State (\s -> (s,ret s)))

{-# INLINE put #-}
put :: s -> Narrative (State s) c ()
put !x = buildn $ \ret _ d -> d (State (\_ -> (x,ret ())))

{-# INLINE modify #-}
modify :: (s -> s) -> Narrative (State s) c s
modify f = buildn $ \ret _ d -> d (State (\s -> let s' = f s in (s',ret s')))

addReturn :: Test Sync ()
addReturn = scope "add return" $ do
  br1 <- nf "mtl" go' 1
  notep br1

  br2 <- nf "ef" go 1
  notep br2

  br3 <- nf "mtl/fmap" go1' (1 :: Int)
  notep br3

  br4 <- nf "ef/fmap" go1 (1 :: Int)
  notep br4

  where
    go = runIdentity . evalState ef_add_return
    go' = runIdentity . St.evalStateT mtl_add_return
    go1 = runIdentity . evalState (fmap (+1) . fmap (+1) . fmap (+1) $ return (3 :: Int))
    go1' = runIdentity . St.evalStateT (fmap (+1) . fmap (+1) . fmap (+1) $ return (3 :: Int))
