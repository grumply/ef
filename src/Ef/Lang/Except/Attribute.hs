{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Ef.Lang.Except.Attribute where



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
    ( Does Attribute contexts
    , Monad environment
    )
