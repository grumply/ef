{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Scoped.Exit
    ( module Ef.Lang.Scoped.Exit.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Lang.Scoped.Exit.Lexemes
import qualified Ef.Lang.Scoped.Exit.Context as Context



instance Inflection Context.Exits Exit
  where

    inflect use (Context.Exits i k) (FreshScope ik) =
        use k (ik i)
