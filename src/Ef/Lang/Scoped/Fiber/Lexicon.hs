{-# LANGUAGE GADTs #-}
module Ef.Lang.Scoped.Fiber.Lexicon
    ( Fiber(..)
    , module Ef.Lang.Scoped.Fiber.Operation
    ) where



import Ef.Core.Narrative

import Ef.Lang.Scoped.Fiber.Operation



data Fiber k
  where

    Fork
        :: Int
        -> Operation status result
        -> Narrative scope parent result
        -> Fiber k

    Yield
        :: Int
        -> Fiber k

    Focus
        :: Int
        -> Narrative scope parent result
        -> Fiber k

    FreshScope
        :: (Int -> k)
        -> Fiber k
