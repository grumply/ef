{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Scoped.Fiber
    ( module Ef.Lang.Scoped.Fiber.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Lang.Scoped.Fiber.Lexemes
import qualified Ef.Lang.Scoped.Fiber.Context as Context



instance Inflection Context.Fibers Fiber
  where

    inflect use (Context.Fibers i k) (FreshScope ik) =
        use k (ik i)
