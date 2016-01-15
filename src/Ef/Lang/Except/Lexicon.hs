{-# LANGUAGE GADTs #-}
module Ef.Lang.Except.Lexicon
    ( Except(..)
    ) where



import Ef.Core.Narrative

import Control.Exception (SomeException(..))



data Except k
  where

    Throw
        :: SomeException
        -> k
        -> Except k
