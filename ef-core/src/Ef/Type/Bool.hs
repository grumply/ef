{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Ef.Type.Bool where

type family And (x :: Bool) (y :: Bool) :: Bool where
  And 'True 'True = 'True
  And x y = 'False

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or x 'True = 'True
  Or x y = 'False

type family (x :: k) === (y :: k) :: Bool where
  x === x = 'True
  x === y = 'False

