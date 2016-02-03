{-# LANGUAGE GADTs #-}
module Ef.Lang.Scoped.Exit.Attribute
    ( Exits(..)
    ) where



data Exits k
  where

    Exits
        :: Int
        -> k
        -> Exits k
