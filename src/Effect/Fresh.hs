-- module Control.Eff.Fresh( Fresh (..)
--                         , fresh
--                         , runFresh
--                         ) where

-- import Data.Typeable

-- import Control.Eff

-- -- | Create unique Enumerable values.
-- newtype Fresh i v = Fresh (i -> v)
--     deriving (Functor, Typeable)

-- -- | Produce a value that has not been previously produced.
-- fresh :: (Typeable i, Enum i, Member (Fresh i) r) => Eff r i
-- fresh = send . inj $ Fresh id

-- -- | Run an effect requiring unique values.
-- runFresh :: (Typeable i, Enum i) => Eff (Fresh i :> r) w -> i -> Eff r w
-- runFresh m s0 = loop s0 m
--   where
--     loop s = freeMap
--              return
--              (\u -> handleRelay u (loop s) $
--                     \(Fresh k) -> (loop $! succ s) (k s))
module Effect.Fresh where

import Mop

data Fresh i k = Fresh (i -> k)
data Freshness i k = Freshness i k

fresh :: Has (Fresh i) fs m => Plan fs m i
fresh = symbol (Fresh id)

freshness :: forall f instrs m.
             (Enum f,Uses (Freshness f) instrs m)
          => f -> Instruction (Freshness f) instrs m
freshness f0 = Freshness f0 $ \is ->
  let Freshness (f :: f) k = view is
  in instruction (Freshness (succ f) k) is

instance Pair (Freshness i) (Fresh i) where
  pair p (Freshness i k) (Fresh ik) = p k (ik i)
