{-# LANGUAGE GADTs #-}
module Ef.Context.Set.Lexicon
    ( Set(..)
    ) where


import Ef.Core.Object
import Ef.Core.Narrative



data Set k
    where

        Set
            :: Object contexts environment
            -> k
            -> Set k
