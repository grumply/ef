{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Effect.Reader.Local where

import Mop
import Effect.Local

data Localize r k = Localize (r -> r) -> k
data Localizer r k = Localizer ((r -> r) -> k)

localize :: Has (Localize r) fs m => (r -> r) -> Plan fs m ()
localize f = symbol (Localize f ())

local :: (Has (Localize r) fs m
         ,Has (Reader r) fs m
         ) => (r -> r) -> Plan fs m a -> Plan fs m a
local f p = do
  orig <- ask
  localize f
  a <- p
  overwrite (const orig)
  return a

localizer :: (Uses (Localizer r) fs m, Uses (Env r) fs m)
          => Instruction (Localizer r) fs m
localizer = Localizer $ \f fs ->
  let Env r k = view fs
  in instruction (Env (f r) k) fs

instance Pair (Localizer r) (Localize r) where
  pair p (Localizer rrk) (Localize rr k) = pair p rrk (rr,k)

