{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Ef.Lang.Except.Lexicon
    ( Lexicon(..)
    , Excepts
    ) where



import Ef.Core.Narrative

import Control.Exception (SomeException(..))



data Lexicon k
  where

    Throw
        :: SomeException
        -> k
        -> Lexicon k



type Excepts lexicon environment =
    ( Can Lexicon lexicon
    , Monad environment
    )
