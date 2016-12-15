{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Ef.Type.Set where

import Ef.Type.Bool
import GHC.TypeLits

type family xs ∪ ys where
    '[] ∪ ys = ys
    '[x] ∪ (x ': xs) = x ': xs
    '[x] ∪ (y ': xs) = y ': '[x] ∪ xs
    (x ': xs) ∪ ys = '[x] ∪ (xs ∪ ys)

type family (x :: k) ∈ (xs :: [k]) :: Bool where
    x ∈ '[] = 'False
    x ∈ (x ': xs) = 'True
    x ∈ (y ': xs) = x ∈ xs

type family (x :: k) ≠ (y :: k) :: Bool where
  x ≠ x = 'False
  x ≠ y = 'True

type family (x :: k) ∉ (ys :: [k]) :: Bool where
  x ∉ '[] = 'True
  x ∉ (x ': ys) = 'False
  x ∉ (y ': ys) = x ∉ ys

type family (xs :: [k]) ⊆ (ys :: [k]) :: Bool where
  '[] ⊆ ys = 'True
  (x ': xs) ⊆ ys = And (x ∈ ys) (xs ⊆ ys)

