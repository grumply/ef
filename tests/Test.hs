{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Ef
import Ef.Interpreter

import Data.Functor.Identity

main3 :: IO ()
main3 = void $ do
  let rdo = mdo
        z <- interp $ prompt "Number" ((+ n) . read)
        x <- return y
        y <- return (3 :: Int)
        n <- interp $ prompt "Number" read
        return (z + x + y + n)
  interpret rdo instr

prompt str f = send (Put str ()) >> send (Get f)

main1 :: IO ()
main1 = do

  (n,t) <- instr $ do
    number <- prompt "What number?" id
    times  <- prompt "How many times?" id
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
    eval (Put str k) = putStr (str ++ ": ") >> k

data StackInstruction k where
  Push :: Int -> k -> StackInstruction k
  Pop  :: (Int -> k) -> StackInstruction k
  deriving Functor

stack = thread eval
  where
    eval (Push a k) stack   = k (a:stack)
    eval (Pop ak) (a:stack) = ak a stack

