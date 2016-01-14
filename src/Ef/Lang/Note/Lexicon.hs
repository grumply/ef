{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Ef.Lang.Note.Lexicon where



import Ef.Core.Narrative



data Lexicon r k
  where

    Note
        :: r
        -> k
        -> Lexicon r k



type Notes r lexicon environment =
    Knows (Lexicon r) lexicon environment
