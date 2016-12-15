{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Ef.Type.List where

import Ef.Type.Nat

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
