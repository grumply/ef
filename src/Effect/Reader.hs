module Effect.Reader
    ( Env, ask, asks, local
    , Reader, reader
    ) where

import Mop
import Unsafe.Coerce

data Env r k = Env (r -> k)

data Reader r k = Reader r k

instance Pair (Reader r) (Env r) where
    pair p (Reader r k) (Env rk) = pair p (r,k) rk

ask :: Has (Env r) fs m => Plan fs m r
ask = symbol (Env id)

asks :: Has (Env r) fs m => (r -> a) -> Plan fs m a
asks f = symbol (Env f)

reader :: Uses (Reader r) fs m => r -> Attribute (Reader r) fs m
reader r = Reader r pure

local :: forall fs m r. Has (Env r) fs m => (r -> r) -> Plan fs m r -> Plan fs m r
local f p0 = go p0 where
    go p = case p of
        Step sym bp -> case prj sym of
            Just (Env (r :: r -> b)) -> Step (inj (Env (r . f)))
                                             (\b -> go (bp b))
            Nothing -> Step sym (\b -> go (bp b))
        M m -> M (fmap go m)
        Pure r -> Pure r
