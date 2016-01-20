{-# LANGUAGE GADTs #-}
module Ef.Context.View.Lexicon
    ( View(..)
    ) where



data View r k
  where

    View
        :: (r -> k)
        -> View r k
