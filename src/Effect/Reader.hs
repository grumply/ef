{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Effect.Reader
  ( Reader, ask, reader, asks
  , Env, env
  ) where

import Mop

data Reader r k = Reader (r -> k)

data Env r k = Env r k

instance Pair (Env r) (Reader r) where
  pair p (Env r k) (Reader rk) = pair p (r,k) rk

ask :: Has (Reader r) fs m => Plan fs m r
ask = symbol (Reader id)

reader :: Has (Reader r) fs m => (r -> a) -> Plan fs m a
reader f = symbol (Reader f)

asks :: Has (Reader r) fs m => (r -> a) -> Plan fs m a
asks = reader

env :: Uses (Env r) fs m => r -> Instruction (Env r) fs m
env r = Env r pure
