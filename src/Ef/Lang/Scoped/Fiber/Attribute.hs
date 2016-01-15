{-# LANGUAGE GADTs #-}
module Ef.Lang.Scoped.Fiber.Attribute
    ( Fibers(..)
    ) where



data Fibers k
  where

    Fibers
        :: Int
        -> k
        -> Fibers k

