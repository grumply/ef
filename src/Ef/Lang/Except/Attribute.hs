{-# LANGUAGE GADTs #-}
module Ef.Lang.Except.Attribute
    ( Excepts(..)
    ) where



import Ef.Core.Object

import Control.Exception (SomeException(..))



data Excepts k
  where

    Excepts
        :: (    SomeException
             -> k
           )
        -> Excepts k


