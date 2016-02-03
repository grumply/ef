{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE KindSignatures #-}
module Ef.Nat where



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



type family Offset (x :: k) (xs :: [k]) :: Nat
  where

    Offset x (x ': xs) =
        'Z

    Offset x (any ': xs) =
        'S (Offset x xs)
