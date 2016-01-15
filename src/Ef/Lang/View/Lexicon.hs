{-# LANGUAGE GADTs #-}
module Ef.Lang.View.Lexicon
    ( View(..)
    ) where



data View r k
  where

    View
        :: (r -> k)
        -> View r k
