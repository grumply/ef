{-
Reader here has been partitioned into the default encapsulation effect, Reader,
and the subcomputation localization effect, Localize. This division is explained
in doc/Effect/Reader.hs
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Effect.Reader
  ( Reader, ask, reader, asks
  , Env, env
  , Localize, local
  , Localizer, localizer
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

data Localize r k = Localize (r -> r) k
data Localizer r k = Localizer ((r -> r) -> k)

overwrite :: Has (Localize r) fs m => (r -> r) -> Plan fs m ()
overwrite f = symbol (Localize f ())

local :: forall r fs m a.
         (Has (Localize r) fs m
         ,Has (Reader r) fs m
         ) => (r -> r) -> Plan fs m a -> Plan fs m a
local f p = do
  orig <- ask
  overwrite f
  a <- p
  overwrite (const (orig :: r))
  return a

localizer :: (Uses (Localizer r) fs m, Uses (Env r) fs m)
          => Instruction (Localizer r) fs m
localizer = Localizer $ \f fs ->
  let Env r k = view fs
  in instruction (Env (f r) k) fs

instance Pair (Localizer r) (Localize r) where
  pair p (Localizer rrk) (Localize rr k) = pair p rrk (rr,k)
