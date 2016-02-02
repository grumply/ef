{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Ef.Core
    ( module Core
    , delta
    , delta'
    , ($.)
    , (#)
    , (#.)
    , Exception(..)
    , SomeException
    ) where

import Ef.Core.Type.Set as Core
import Ef.Core.Type.Nat as Core

import Ef.Core.Object as Core
import Ef.Core.Object.Context as Core

import Ef.Core.Narrative as Core
import Ef.Core.Narrative.Lexicon as Core
import Ef.Core.Narrative.Exception as Core

import Ef.Core.Inflect as Core

import Control.Exception (Exception(..),SomeException)
import qualified Control.Exception as Exception
import Data.Typeable (Typeable,typeOf)
import Debug.Trace
import Unsafe.Coerce


import GHC.Exts

-- | Send a narrative to an object for invocation; returns a new, modified object
-- and a result.
--
-- >    (resultObj,result) <- delta obj narrative
delta
    :: ( Inflections contexts lexicon
       , Monad environment
       )
    => Object contexts environment
    -> Narrative lexicon environment result
    -> environment (Object contexts environment,result)
delta =
    _delta



infixr 5 $.
-- | Synonym for 'delta'. Send a narrative to an object.
--
-- >    (resultObj,result) <- obj $. narrative
($.)
    :: ( Inflections contexts lexicon
       , Monad environment
       )
    => Object contexts environment
    -> Narrative lexicon environment result
    -> environment (Object contexts environment,result)
    
($.) = delta



infixl 5 #
-- | Like 'delta' for objects in an environment, but without a return value.
-- Permits a chaining syntax:
--
-- >    resultObj <- pure obj # method1 # method2 # method3
(#)
    :: ( Inflections contexts lexicon
       , Monad environment
       )
    => environment (Object contexts environment)
    -> Narrative lexicon environment result
    -> environment (Object contexts environment)
(#) obj passage = fmap fst (obj #. passage)



-- | Like 'delta' for objects in an environment. Like '#', but can be used to end
-- a chain of method calls, for example:
--
-- >    (resultObj,result) <- pure obj # method1 # method2 #. method3
(#.)
    :: ( Inflections contexts lexicon
       , Monad environment
       )
    => environment (Object contexts environment)
    -> Narrative lexicon environment result
    -> environment (Object contexts environment,result)

(#.) obj passage = obj >>= ($. passage)



-- | Like 'delta' for pairing a narrative with an object that permits more capabilities
-- than those prescribed by the narrative.
--
-- >    (resultObj,result) <- delta' obj smallNarrative
delta'
    :: ( Inflections contexts lexicon'
       , Grow (Lexicon lexicon) (Lexicon lexicon')
       , Monad environment
       )
    => Object contexts environment
    -> Narrative lexicon environment result
    -> environment (Object contexts environment,result)
delta' o =
    _delta o . grow



{-# INLINE _delta #-}
_delta
    :: ( Inflections contexts lexicon
       , Monad environment
       )
    => Object contexts environment
    -> Narrative lexicon environment result
    -> environment (Object contexts environment,result)
_delta object =
    go
  where

      go (Say symbol k) =
          let
              !(method,b) =
                  inflect (,) (deconstruct object) symbol

          in
              do
                  !object' <- method object
                  _delta object' (k b)

      go (Fail e) =
          Exception.throw e

      go (Super m) =
          m >>= go

      go (Return result) =
          pure (object,result)



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


{-# INLINE delta #-}
