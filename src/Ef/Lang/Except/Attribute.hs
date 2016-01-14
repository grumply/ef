{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Ef.Lang.Except.Attribute
    ( Attribute(..)
    , Excepts
    ) where



import Ef.Core.Object

import Control.Exception (SomeException(..))



data Attribute k
  where

    Except
        :: (    SomeException
             -> k
           )
        -> Attribute k



type Excepts contexts environment =
    Has Attribute contexts environment
