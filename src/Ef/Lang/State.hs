{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.State
    ( module Ef.Lang.State.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Lang.State.Lexemes
import Ef.Lang.State.Context



instance Inflection (Stateful st) (State st)
  where

    inflect use (State st k) (Modify stst stk) =
        let
          st' =
              stst st

        in
          inflect use (st,k st') stk
