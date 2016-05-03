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
    , stream
    , ($..)
    , ($>)
    , ($.)
    ) where

import Ef.Type.Set as Core (Union)

import Ef.Object as Core
import Ef.Traits as Core

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
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super
       )
    => Object traits super
    -> Narrative messages super result
    -> super (Object traits super,result)
delta = _delta
{-# INLINE delta #-}



infixr 5 $.
-- | Synonym for 'delta'. Send a narrative to an object.
--
-- >    (resultObj,result) <- obj $. narrative
($.)
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super
       )
    => Object traits super
    -> Narrative messages super result
    -> super (Object traits super,result)
($.) = _delta
{-# INLINE ($.) #-}



-- | Strict version of ($.)
($..)
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super
       , NFData (Traits traits (Object traits super -> super (Object traits super)))
       )
    => Object traits super
    -> Narrative messages super result
    -> super (Object traits super,result)
($..) obj = go
  where
    go (Fail e) = Exception.throw e

    go (Super sup) = do
        narrative <- sup
        go narrative

    go (Return result) = return (obj,result)

    go (Say message k) = do
        (obj',v) <- obj $. Say message Return
        obj' `deepseq` (obj' $.. k v)
{-# INLINE ($..) #-}



($>) :: ( (Traits traits) `Ma` (Messages messages)
        , Monad super
        )
     => Object traits super
     -> Narrative messages super result
     -> super [(Object traits super, Narrative messages super result)]
($>) = stream
{-# INLINE ($>) #-}



stream :: ( (Traits traits) `Ma` (Messages messages)
          , Monad super
          )
       => Object traits super
       -> Narrative messages super result
       -> super [(Object traits super, Narrative messages super result)]
stream = __delta
{-# INLINE stream #-}



__delta
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super
       )
    => Object traits super
    -> Narrative messages super result
    -> super [(Object traits super, Narrative messages super result)]
__delta = go []
  where
    go acc object (Fail e) = return $ (object,Fail e):acc

    go acc object (Super sup) = do
        narrative <- sup
        go ((object,Super sup):acc) object narrative

    go acc object (Return result) = return $ (object,Return result):acc

    go acc object (Say symbol k) =
        let !(method,b) = ma (,) (deconstruct object) symbol
        in do !object' <- method object
              go ((object,Say symbol k):acc) object'  (k b)
{-# INLINE __delta #-}



_delta
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super
       )
    => Object traits super
    -> Narrative messages super result
    -> super (Object traits super,result)
_delta object = go
  where
    go (Say symbol k) =
        let !(method,b) = ma (,) (deconstruct object) symbol
        in do !object' <- method object
              _delta object' (k b)

    go (Fail e) =
        Exception.throw e

    go (Super m) =
        m >>= go

    go (Return result) = pure (object,result)
{-# INLINE _delta #-}



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


