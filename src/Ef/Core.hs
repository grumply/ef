{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Ef.Core
    ( module Core
    , delta
    , delta'
    , deltaUpcast
    , (#)
    , Exception(..)
    , SomeException
    ) where

import Ef.Core.Type.Set as Core
import Ef.Core.Type.Nat as Core

import Ef.Core.Object as Core
import Ef.Core.Object.Attributes as Core

import Ef.Core.Pattern as Core
import Ef.Core.Pattern.Symbols as Core
import Ef.Core.Pattern.Exception as Core

import Ef.Core.Witness as Core

import Control.Exception (Exception(..),SomeException)
import qualified Control.Exception as Exception
import Data.Typeable (Typeable,typeOf)
import Debug.Trace
import Unsafe.Coerce



(#)
    :: ( Witnessing (Attrs attrs) (Symbol scope)
       , Monad parent
       )
    => parent (Object attrs parent)
    -> Pattern scope parent result
    -> parent (Object attrs parent)
(#) mobj p =
    do
      obj <- mobj
      (obj',_) <- delta obj p
      return obj'



delta'
    :: ( Witnessing (Attrs attrs) (Symbol scope')
       , As (Symbol scope) (Symbol scope')
       , Monad parent
       )
    => Object attrs parent
    -> Pattern scope parent result
    -> parent (Object attrs parent,result)
delta' o =
    _delta o . rearrange



deltaUpcast
    :: ( Witnessing (Attrs attrs) (Symbol large)
       , Cast small large
       , Monad parent
       )
    => Object attrs parent
    -> Pattern small parent result
    -> parent (Object attrs parent,result)
deltaUpcast o =
    _delta o . upcast



delta
    :: ( Witnessing (Attrs attrs) (Symbol scope)
       , Monad parent
       )
    => Object attrs parent
    -> Pattern scope parent result
    -> parent (Object attrs parent,result)
delta =
    _delta



{-# NOINLINE _delta #-}
_delta
    :: ( Witnessing (Attrs attrs) (Symbol scope)
       , Monad parent
       )
    => Object attrs parent
    -> Pattern scope parent result
    -> parent (Object attrs parent,result)
_delta =
    go
  where
    go object = go'
      where
        go' (Fail e) =
            Exception.throw e

        go' (Super m) =
            m >>= go'

        go' (Pure result) =
            pure (object,result)

        go' (Send symbol k) =
            let
              (method,b) =
                  witness (,) (deconstruct object) symbol

            in
              do
                object' <- method object
                go object' (k b)
