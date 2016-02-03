{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Knot
    ( module Ef.Lang.Knot.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Lang.Knot.Context
import Ef.Lang.Knot.Lexemes



instance Inflection Knot Knots
    where

        inflect use (Knot i k) (FreshScope ik) =
            use k (ik i)
