{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Set
    ( module Ef.Lang.Set.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Lang.Set.Lexemes
import Ef.Lang.Set.Context

import Unsafe.Coerce



instance Inflection Sets Set
    where

        inflect use (Sets ok) (Set o k) =
            let
                obj =
                    unsafeCoerce o

            in
                use (ok obj) k
