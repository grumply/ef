module Effect.Reader
  ( Reader, ask, asks, local
  , Env, reader
  ) where

import Mop
import Unsafe.Coerce

data Reader r k = Reader (r -> k)

data Env r k = Env r k

instance Pair (Env r) (Reader r) where
  pair p (Env r k) (Reader rk) = pair p (r,k) rk

ask :: Has (Reader r) fs m => Plan fs m r
ask = symbol (Reader id)

asks :: Has (Reader r) fs m => (r -> a) -> Plan fs m a
asks f = symbol (Reader f)

reader :: Uses (Env r) fs m => r -> Attribute (Env r) fs m
reader r = Env r pure

local :: forall fs m r. Has (Reader r) fs m => (r -> r) -> Plan fs m r -> Plan fs m r
local f p0 = go p0
  where
    go p =
      case p of
        Step sym bp ->
          case prj sym of
            Just (Reader (r :: r -> b)) -> Step (inj (Reader (r . f))) (\b -> go (bp b))
            Nothing -> Step sym (\b -> go (bp b))
        M m -> M (fmap go m)
        Pure r -> Pure r
