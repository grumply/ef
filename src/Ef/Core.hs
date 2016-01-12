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
    , deltaGrow
    , (#)
    , Exception(..)
    , SomeException
    ) where

import Ef.Core.Type.Set as Core
import Ef.Core.Type.Nat as Core

import Ef.Core.Object as Core hiding (_fmap)
import Ef.Core.Object.Context as Core

import Ef.Core.Narrative as Core hiding (_fmap)
import Ef.Core.Narrative.Lexeme as Core
import Ef.Core.Narrative.Exception as Core

import Ef.Core.Inflect as Core

import Control.Exception (Exception(..),SomeException)
import qualified Control.Exception as Exception
import Data.Typeable (Typeable,typeOf)
import Debug.Trace
import Unsafe.Coerce



(#)
    :: ( Inflection (Context contexts) (Lexeme lexicon)
       , Monad environment
       )
    => environment (Object environment contexts)
    -> Narrative lexicon environment result
    -> environment (Object environment contexts)
(#) mobj p =
    do
      obj <- mobj
      (obj',_) <- delta obj p
      return obj'



delta'
    :: ( Inflection (Context contexts) (Lexeme lexicon')
       , As (Lexeme lexicon) (Lexeme lexicon')
       , Monad environment
       )
    => Object environment contexts
    -> Narrative lexicon environment result
    -> environment (Object environment contexts,result)
delta' o =
    _delta o . rearrange



deltaGrow
    :: ( Inflection (Context contexts) (Lexeme large)
       , Grow small large
       , Monad environment
       )
    => Object environment contexts
    -> Narrative small environment result
    -> environment (Object environment contexts,result)
deltaGrow o =
    _delta o . upembed



delta
    :: ( Inflection (Context contexts) (Lexeme lexicon)
       , Monad environment
       )
    => Object environment contexts
    -> Narrative lexicon environment result
    -> environment (Object environment contexts,result)
delta =
    _delta



{-# NOINLINE _delta #-}
_delta
    :: ( Inflection (Context contexts) (Lexeme lexicon)
       , Monad environment
       )
    => Object environment contexts
    -> Narrative lexicon environment result
    -> environment (Object environment contexts,result)
_delta object =
    go
  where
    go (Fail e) =
        Exception.throw e

    go (Super m) =
        m >>= go

    go (Return result) =
        pure (object,result)

    go (Say symbol k) =
        let
            (method,b) =
                inflect (,) (deconstruct object) symbol

        in
            do
                object' <- interpret object method
                _delta object' (k b)

{-# RULES

    "_delta obj (Fail e)"
        forall obj e.
            _delta obj (Fail e) =
                Exception.throw e

    ;

    "_delta obj (Super m)"
        forall obj m.
            _delta obj (Super m) =
                m >>= _delta obj

    ;

    "_delta obj (Return result)"
        forall obj result.
            _delta obj (Return result) =
                pure (obj,result)

    ;

    "_delta obj (Say symbol k)"
        forall obj symbol k.
            _delta obj (Say symbol k) =
                let
                    (method,b) =
                        inflect (,) (deconstruct obj) symbol

                in
                    do
                        object' <- interpret obj method
                        _delta object' (k b)

  #-}
