{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Effect.For
  ( for
  , forLoops, ForLoopHandler(..)
  ) where

import Mop
import Data.Function

data For k
  = FreshScope (Integer -> k)
  | forall s. Continue Integer s
  | forall a. Break Integer a

freshScope :: Has For fs m => Plan fs m Integer
freshScope = symbol (FreshScope id)

for :: forall fs m s a. (Has For fs m,Foldable f)
    => s
    -> (    (forall b. a -> Plan fs m b)
         -> (forall b. s -> Plan fs m b)
         -> s
         -> x
         -> Plan fs m a
       )
    -> f x
    -> Plan fs m a
for s0 h fx = do
  scope <- freshScope
  let break a = symbol (Break scope a)
      continue s = symbol (Continue scope s)
      loopBody r s x = reifyForLoop r scope (h break continue s x)
