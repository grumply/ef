{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Note
    ( module Ef.Lang.Note.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Lang.Note.Lexemes
import Ef.Lang.Note.Context



instance Inflection (Notes r) (Note r)
  where

    inflect use (Notes _ rk) (Note r k) =
        inflect use rk (r,k)
