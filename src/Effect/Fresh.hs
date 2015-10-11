module Effect.Fresh
  ( fresh, Fresh
  , uniques,
  ) where

import Mop

data Fresh i k = Fresh (i -> k)
data Uniques i k = Uniques i k

fresh :: Has (Fresh i) fs m => Plan fs m i
fresh = symbol (Fresh id)

uniques :: forall f instrs m.
             (Enum f,Uses (Uniques f) instrs m)
          => f -> Instruction (Uniques f) instrs m
uniques f0 = Uniques f0 $ \is ->
  let Uniques (f :: f) k = view is
  in instruction (Uniques (succ f) k) is

instance Pair (Uniques i) (Fresh i) where
  pair p (Uniques i k) (Fresh ik) = p k (ik i)
