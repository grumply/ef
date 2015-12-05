{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE KindSignatures #-}
module Ef.Core.Type.Nat where



data Nat
  where

    Z
        :: Nat

    S
        :: Nat -> Nat



data Index (n :: Nat)
  where

    Index
        :: Index n



type family IndexOf (x :: k) (xs :: [k]) :: Nat
  where

    IndexOf x (x ': xs) =
        'Z

    IndexOf x (any ': xs) =
        'S (IndexOf x xs)
