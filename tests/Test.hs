{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Ef

import Data.Functor.Identity

main = do
  noops (go 100000000)
  where
    go (0 :: Int) = return ()
    go n = do
      send (Noop ())
      go (n - 1)

main' :: IO ()
main' = do
  let prompt str = send (Put str ()) >> send (Get id)

  (n,t) <- instr $ do
    number <- prompt "What number?"
    times  <- prompt "How many times?"
    return (read number,read times)

  (s,_) <- (`stack` []) $
             replicateM_ t $
               send (Push n ())

  print s

data Instr k where
  Get :: (String -> k) -> Instr k
  Put :: String -> k -> Instr k
  deriving Functor

instr = run eval
  where
    eval (Get k)     = getLine >>= k
    eval (Put str k) = putStrLn str >> k

data StackInstruction k where
  Push :: Int -> k -> StackInstruction k
  Pop  :: (Int -> k) -> StackInstruction k
  deriving Functor

stack = thread eval
  where
    eval (Push a k) stack   = k (a:stack)
    eval (Pop ak) (a:stack) = ak a stack

data Noop k where
  Noop :: k -> Noop k
  deriving Functor

noops = run eval
  where
    eval (Noop k) = k
