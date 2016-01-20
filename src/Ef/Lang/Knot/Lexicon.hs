{-# LANGUAGE GADTs #-}
module Ef.Lang.Knot.Lexicon
    ( Knots(..)
    ) where



import Ef.Core.Narrative



data Knots k
  where

    FreshScope
        :: (Int -> k)
        -> Knots k

    Request
        :: Int
        -> a'
        -> (a  -> Narrative lexicon environment r)
        -> Knots k

    Respond
        :: Int
        -> b
        -> (b' -> Narrative lexicon environment r)
        -> Knots k
