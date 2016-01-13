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
    , (#)
    , Exception(..)
    , SomeException
    ) where

import Ef.Core.Type.Set as Core
import Ef.Core.Type.Nat as Core

import Ef.Core.Object as Core hiding (_fmap)
import Ef.Core.Object.Context as Core

import Ef.Core.Narrative as Core hiding (_fmap)
import Ef.Core.Narrative.Lexicon as Core
import Ef.Core.Narrative.Exception as Core

import Ef.Core.Inflect as Core

import Control.Exception (Exception(..),SomeException)
import qualified Control.Exception as Exception
import Data.Typeable (Typeable,typeOf)
import Debug.Trace
import Unsafe.Coerce



-- | Pair an Object with a Narrative. This is the equivalent of OOP's
-- 'send a message to an object' or 'invoke a method'. 
delta
    :: ( Inflection (Context contexts) (Lexicon lexicon)
       , Monad environment
       )
    => Object contexts environment
    -> Narrative lexicon environment result
    -> environment (Object contexts environment,result)
delta =
    _delta



infixl 5 #
-- | Like 'delta' without a return value that permits a chaining syntax.
--  resultObj <- pure obj # method1 # method2 # method3
(#)
    :: ( Inflection (Context contexts) (Lexicon lexicon)
       , Monad environment
       )
    => environment (Object contexts environment)
    -> Narrative lexicon environment result
    -> environment (Object contexts environment)
(#) mobj p =
    do
      obj <- mobj
      (obj',_) <- delta obj p
      return obj'



-- | Don't use this; rerrange your constructor.
delta'
    :: ( Inflection (Context contexts) (Lexicon lexicon')
       , Grow (Lexicon lexicon) (Lexicon lexicon')
       , Monad environment
       )
    => Object contexts environment
    -> Narrative lexicon environment result
    -> environment (Object contexts environment,result)
delta' o =
    _delta o . grow



{-# NOINLINE _delta #-}
_delta
    :: ( Inflection (Context contexts) (Lexicon lexicon)
       , Monad environment
       )
    => Object contexts environment
    -> Narrative lexicon environment result
    -> environment (Object contexts environment,result)
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
                object' <- method object
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
                        object' <- method obj
                        _delta object' (k b)

  #-}
