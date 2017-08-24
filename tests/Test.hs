{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Ef

import Data.Functor.Identity

import Data.Functor.Identity

main = print $ runIdentity $ ef_add_return 1

{-# INLINE ef_add_return #-}
ef_add_return :: Int -> Identity Int
ef_add_return n = (`evalState` n) $ do
  x :: Int <- get
  y :: Int <- get
  put $ x + y
  get

data State s k
  = Get (s -> k)
  | Put s k
  deriving Functor

{-# INLINE evalState #-}
evalState :: Monad c => Narrative (State s) c a -> s -> c a
evalState n = foldn (\a _ -> return a) (\cf a -> cf >>= ($ a)) d n
  where
    d (Get sk) s = sk s s
    d (Put s' k) _ = k s'

{-# INLINE get #-}
get = buildn $ \r _ d -> d (Get r)

{-# INLINE put #-}
put x = buildn $ \r _ d -> d (Put x (r ()))

