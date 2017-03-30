{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Ef.Type.List where

import Ef.Type.Nat

import GHC.Exts

data Index (n :: Nat) where
  Index :: Index n

type family Appended (xs :: [k]) (ys :: [k]) where
  Appended '[] ys = ys
  Appended xs '[] = xs
  Appended (x ': xs) ys = x ': (Appended xs ys)

type family Offset (xs :: [k]) (x :: k) :: Nat where
  Offset (x ': xs) x = 'Z
  Offset (any ': xs) x = 'S (Offset xs x)

type family Removed xs x where
  Removed '[] x = '[]
  Removed (x ': xs) x = xs
  Removed (any ': xs) x = any ': Removed xs x

type family Constrain cs as :: Constraint where
  Constrain '[] as = ()
  Constrain (c ': cs) as = (Constrained c as, Constrain cs as)

type family Constrained c as :: Constraint where
  Constrained c '[] = ()
  Constrained c (a ': as) = (c a, Constrained c as)
