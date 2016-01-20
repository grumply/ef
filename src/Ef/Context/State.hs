{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Context.State
    ( module Ef.Context.State.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Context.State.Lexemes
import Ef.Context.State.Context



instance Inflection (Stateful st) (State st)
  where

    inflect use (State st k) (Modify stst stk) =
        let
          st' =
              stst st

        in
          inflect use (st,k st') stk
