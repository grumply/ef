{-# LANGUAGE GADTs #-}
module Ef.Lang.Knot.Attribute
    ( Knot(..)
    ) where



data Knot k
  where

    Knot
        :: Int
        -> k
        -> Knot k
