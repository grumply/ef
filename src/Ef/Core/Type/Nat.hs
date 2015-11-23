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



type family IndexOf (f :: k) (fs :: [k]) :: Nat
  where

    IndexOf f (f ': fs) =
        'Z

    IndexOf f (any ': fs) =
        'S (IndexOf f fs)
