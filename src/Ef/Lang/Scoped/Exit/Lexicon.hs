{-# LANGUAGE GADTs #-}
module Ef.Lang.Scoped.Exit.Lexicon
    ( Exit(..)
    ) where



data Exit k
  where

    FreshScope
        :: (Int -> k)
        -> Exit k

    Done
        :: Int
        -> a
        -> Exit k

