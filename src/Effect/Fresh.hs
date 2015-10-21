module Effect.Fresh
  ( fresh, Fresh
  , uniques,
  ) where

import Mop

data Fresh i k = Fresh (i -> k)
data Uniques i k = Uniques i k

fresh :: Has (Fresh i) fs m => Plan fs m i
fresh = symbol (Fresh id)

uniques :: forall f fs m. (Enum f,Uses (Uniques f) fs m)
          => f -> Attribute (Uniques f) fs m
uniques f0 = Uniques f0 $ \is ->
  let Uniques (f :: f) k = (is&)
  in pure $ is .= Uniques (succ f) k

instance Pair (Uniques i) (Fresh i) where
  pair p (Uniques i k) (Fresh ik) = p k (ik i)
