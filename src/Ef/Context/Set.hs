{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Context.Set
    ( module Ef.Context.Set.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Context.Set.Lexemes
import Ef.Context.Set.Context

import Unsafe.Coerce



instance Inflection Sets Set
    where

        inflect use (Sets ok) (Set o k) =
            let
                obj =
                    unsafeCoerce o

            in
                use (ok obj) k
