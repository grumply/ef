{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Mop
import Effect.State

-- lazyState :: (Has (State Integer) fs m) => Plan fs m [Integer]
-- lazyState = mapM (\n -> modify (n+) >> get) [1 :: Integer ..]

-- main' = do
--   (_,i :: Integer) <- delta (Instructions $ store (0 :: Integer) *:* Empty) (cutoff 15 lazyState >> get)
--   print (i == 15)

{-# INLINE countdown #-}
countdown :: Has (State Int) fs m => Int -> Plan fs m Int
countdown = go
  where
    go 0 = get
    go n = do
      modify (+(1 :: Int))
      go (n - 1)

main = do
  (_,i :: Int) <- delta (Instructions $ store (0 :: Int) *:* Empty) (countdown 10000000)
  print i
