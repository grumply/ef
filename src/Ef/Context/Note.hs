{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Context.Note
    ( module Ef.Context.Note.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Context.Note.Lexemes
import Ef.Context.Note.Context



instance Inflection (Notes r) (Note r)
  where

    inflect use (Notes _ rk) (Note r k) =
        inflect use rk (r,k)
