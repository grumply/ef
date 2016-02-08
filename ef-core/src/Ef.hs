{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE Trustworthy #-}
module Ef
    ( module Core
    , delta
    , delta'
    , stream
    , ($..)
    , ($>)
    , ($.)
    , (#)
    , (#.)
    ) where

import Ef.Type.Set as Core (Union)

import Ef.Object as Core
import Ef.Methods as Core

import Ef.Narrative as Core
import Ef.Messages as Core
import Ef.Exception as Core

import Ef.Ma as Core

import Control.DeepSeq

import qualified Control.Exception as Exception

-- | Send a narrative to an object for invocation; returns a new, modified object
-- and a result.
--
-- >    (resultObj,result) <- delta obj narrative
delta
    :: ( (Methods methods) `Ma` (Messages self)
       , Monad super
       )
    => Object methods super
    -> Narrative self super result
    -> super (Object methods super,result)
delta =
    _delta
{-# INLINE delta #-}


infixr 5 $.
-- | Synonym for 'delta'. Send a narrative to an object.
--
-- >    (resultObj,result) <- obj $. narrative
($.)
    :: ( (Methods methods) `Ma` (Messages self)
       , Monad super
       )
    => Object methods super
    -> Narrative self super result
    -> super (Object methods super,result)
   
($.) = delta

($..)
    :: ( (Methods methods) `Ma` (Messages self)
       , Monad super
       , NFData (Methods methods (Implementation methods super))
       )
    => Object methods super
    -> Narrative self super result
    -> super (Object methods super,result)

($..) = delta__

delta__
    :: ( (Methods methods) `Ma` (Messages self)
       , Monad super
       , NFData (Methods methods (Implementation methods super))
       )
    => Object methods super
    -> Narrative self super result
    -> super (Object methods super,result)
delta__ obj =
    go
    where

        go (Fail e) = Exception.throw e
        go (Super sup) = do
            narrative <- sup
            go narrative
        go (Return result) = return (obj,result)
        go (Say message k) = do
            (obj',v) <- obj $. (Say message Return)
            obj' `deepseq` delta__ obj' (k v)
infixl 5 #
-- | Like 'delta' for objects in an super, but without a return value.
-- Permits a chaining syntax:
--
-- >    resultObj <- pure obj # method1 # method2 # method3
(#)
    :: ( (Methods methods) `Ma` (Messages self)
       , Monad super
       )
    => super (Object methods super)
    -> Narrative self super result
    -> super (Object methods super)
(#) obj passage = fmap fst (obj #. passage)



-- | Like 'delta' for objects in an super. Like '#', but can be used to end
-- a chain of method calls, for example:
--
-- >    (resultObj,result) <- pure obj # method1 # method2 #. method3
(#.)
    :: ( (Methods methods) `Ma` (Messages self)
       , Monad super
       )
    => super (Object methods super)
    -> Narrative self super result
    -> super (Object methods super,result)

(#.) obj passage = obj >>= ($. passage)



-- | Like 'delta' for pairing a narrative with an object that permits more capabilities
-- than those prescribed by the narrative.
--
-- >    (resultObj,result) <- delta' obj smallNarrative
delta'
    :: ( (Methods methods) `Ma` (Messages self')
       , Upcast (Messages self) (Messages self')
       , Monad super
       )
    => Object methods super
    -> Narrative self super result
    -> super (Object methods super,result)
delta' o =
    _delta o . upcast



($>) :: ( (Methods methods) `Ma` (Messages self)
        , Monad super
        )
     => Object methods super
     -> Narrative self super result
     -> super ([(Object methods super,Narrative self super result)])

($>) = stream

stream :: ( (Methods methods) `Ma` (Messages self)
          , Monad super
          )
       => Object methods super
       -> Narrative self super result
       -> super ([(Object methods super,Narrative self super result)])

stream = __delta

{-# INLINE __delta #-}
__delta
    :: ( (Methods methods) `Ma` (Messages self)
       , Monad super
       )
    => Object methods super
    -> Narrative self super result
    -> super ([(Object methods super,Narrative self super result)])
__delta =
    go []
    where
        go acc object (Fail e) = do
            return $ (object,Fail e):acc
        go acc object (Super sup) = do
            narrative <- sup
            go ((object,Super sup):acc) object narrative
        go acc object (Return result) = do
            return $ (object,Return result):acc
        go acc object (Say symbol k) =
            let !(method,b) = ma (,) (deconstruct object) symbol
            in do !object' <- method object
                  let narrative = k b
                  go ((object,Say symbol k):acc) object' narrative


{-# INLINE _delta #-}
_delta
    :: ( (Methods methods) `Ma` (Messages self)
       , Monad super
       )
    => Object methods super
    -> Narrative self super result
    -> super (Object methods super,result)
_delta object =
    go
  where

      go (Say symbol ~k) =
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

      go (Return result) = pure (object,result)



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


