{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Test where

import Ef
import Ef.Interpreter as I

import Data.Functor.Identity

main :: IO ()
main = void $ do
  let rdo = mdo
        z <- prompt "Number" ((+ n) . read)
        x <- return y
        y <- return (3 :: Int)
        n <- prompt "Number" read
        return (z + x + y + n)
  instr rdo

prompt str f = I.send (Put str ()) >> I.send (Get f)

main1 :: IO ()
main1 = do

  (n,t) <- instr $ do
    number <- prompt "What number?" id
    times  <- prompt "How many times?" id
    return (read number,read times)

  (s,_) <- (`stack` []) $
             replicateM_ t $
               I.send (Push n ())

  print s

data Instr k where
  Get :: (String -> k) -> Instr k
  Put :: String -> k -> Instr k
  deriving Functor

instr = I.run (Ef.run eval)
  where
    eval (Get k)     = getLine >>= k
    eval (Put str k) = putStr (str ++ ": ") >> k

data StackInstruction k where
  Push :: Int -> k -> StackInstruction k
  Pop  :: (Int -> k) -> StackInstruction k
  deriving Functor

stack = I.thread (Ef.thread eval)
  where
    eval (Push a k) stack   = k (a:stack)
    eval (Pop ak) (a:stack) = ak a stack

