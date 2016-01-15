{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Vary
    ( module Ef.Lang.Vary.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Lang.Vary.Lexemes
import Ef.Lang.Vary.Context



instance Inflection (Varies st) (Vary st)
  where

    inflect use (Varies st k) (Modify stst stk) =
        let
          st' =
              stst st

        in
          inflect use (st,k st') stk
