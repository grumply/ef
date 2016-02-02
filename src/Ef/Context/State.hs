{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Context.State
    ( module Ef.Context.State.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Context.State.Lexemes
import Ef.Context.State.Context



instance Inflection (Stateful st) (State st)
  where

    inflect use (State st stk) (Modify stst stk') =
        let
          st' =
              stst st
              
          k =
              stk st'

          k' =
              stk' st

        in
          use k k'
