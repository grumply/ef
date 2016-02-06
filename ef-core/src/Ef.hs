{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE Trustworthy#-}
module Ef
    ( module Core
    , delta
    , delta'
    , ($.)
    , (#)
    , (#.)
    ) where

import Ef.Set as Core
import Ef.Nat as Core

import Ef.Object as Core
import Ef.Methods as Core

import Ef.Narrative as Core
import Ef.Messages as Core
import Ef.Exception as Core

import Ef.Ma as Core

import Debug.Trace
import Unsafe.Coerce

import qualified Control.Exception as Exception

import GHC.Exts

-- | Send a narrative to an object for invocation; returns a new, modified object
-- and a result.
--
-- >    (resultObj,result) <- delta obj narrative
delta
    :: ( (Methods methods) `Ma` (Messages messages)
       , Monad supertype
       )
    => Object methods supertype
    -> Narrative messages supertype result
    -> supertype (Object methods supertype,result)
delta =
    _delta



infixr 5 $.
-- | Synonym for 'delta'. Send a narrative to an object.
--
-- >    (resultObj,result) <- obj $. narrative
($.)
    :: ( (Methods methods) `Ma` (Messages messages)
       , Monad supertype
       )
    => Object methods supertype
    -> Narrative messages supertype result
    -> supertype (Object methods supertype,result)
    
($.) = delta



infixl 5 #
-- | Like 'delta' for objects in an supertype, but without a return value.
-- Permits a chaining syntax:
--
-- >    resultObj <- pure obj # method1 # method2 # method3
(#)
    :: ( (Methods methods) `Ma` (Messages messages)
       , Monad supertype
       )
    => supertype (Object methods supertype)
    -> Narrative messages supertype result
    -> supertype (Object methods supertype)
(#) obj passage = fmap fst (obj #. passage)



-- | Like 'delta' for objects in an supertype. Like '#', but can be used to end
-- a chain of method calls, for example:
--
-- >    (resultObj,result) <- pure obj # method1 # method2 #. method3
(#.)
    :: ( (Methods methods) `Ma` (Messages messages)
       , Monad supertype
       )
    => supertype (Object methods supertype)
    -> Narrative messages supertype result
    -> supertype (Object methods supertype,result)

(#.) obj passage = obj >>= ($. passage)



-- | Like 'delta' for pairing a narrative with an object that permits more capabilities
-- than those prescribed by the narrative.
--
-- >    (resultObj,result) <- delta' obj smallNarrative
delta'
    :: ( (Methods methods) `Ma` (Messages messages')
       , Upcast (Messages messages) (Messages messages')
       , Monad supertype
       )
    => Object methods supertype
    -> Narrative messages supertype result
    -> supertype (Object methods supertype,result)
delta' o =
    _delta o . upcast



{-# INLINE _delta #-}
_delta
    :: ( (Methods methods) `Ma` (Messages messages)
       , Monad supertype
       )
    => Object methods supertype
    -> Narrative messages supertype result
    -> supertype (Object methods supertype,result)
_delta object =
    go
  where

      go (Say symbol k) =
          let
              !(method,b) =
                  ma (,) (deconstruct object) symbol

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
                        ma (,) (deconstruct obj) symbol

                in
                    do
                        object' <- method obj
                        _delta object' (k b)

  #-}


{-# INLINE delta #-}
