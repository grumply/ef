{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Mop
import Effect.State
import Effect.Reader

import Data.Functor.Identity


-- example taken from https://mail.haskell.org/pipermail/libraries/2011-September/016768.html
lazy is = foldP is (\n -> modify' (n+) >> get) [1 :: Integer .. ]

lazyTest = (snd . (!! 10000000) :: [(a,Integer)] -> Integer)
         $ runIdentity
         $ lazy
         $ Instructions
         $ store (0 :: Integer) *:* Empty

stateReader = do
  r <- ask
  st <- get
  return (r,st)

stateReaderI = Instructions $ store (0 :: Int) *:* env (1 :: Int) *:* Empty

stateReaderTest = snd
                $ runIdentity
                $ delta stateReaderI stateReader

main :: IO ()
main = print stateReaderTest
