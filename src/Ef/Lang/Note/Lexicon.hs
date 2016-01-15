{-# LANGUAGE GADTs #-}
module Ef.Lang.Note.Lexicon
    ( Note(..)
    ) where



import Ef.Core.Narrative



data Note r k
  where

    Note
        :: r
        -> k
        -> Note r k
