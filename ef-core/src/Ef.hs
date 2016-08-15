{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-inline-rule-shadowing #-}
module Ef
    ( module Core
    , delta
    , delta_
    , ($.)
    , ($!.)
    , ($..)
    , ($!..)
    ) where

import Ef.Type.Set as Core (type (/==),Union)

import Ef.Type.Nat as Core (Offset)

import Ef.Object as Core
import Ef.Traits as Core

import Ef.Narrative as Core
import Ef.Messages as Core
import Ef.Exception as Core

import Ef.Codensity as Core

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
{-# INLINE [2] delta #-}


-- | Send a narrative to an object for invocation in the context of another narrative;
-- returns a new, modified object and a result. Exceptions are lifted to the parent.
--
-- >    delta parent $ do
-- >      (resultObj,result) <- delta_ obj narrative
delta_
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super
       )
    => Object traits (Narrative self super)
    -> Narrative messages (Narrative self super) result
    -> Narrative self super (Object traits (Narrative self super),result)
delta_ = __delta
{-# INLINE [2] delta_ #-}



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
{-# INLINE [2] ($.) #-}


infixr 5 $..
-- | Synonym for 'delta_'. Send a narrative to an object in
-- the context of another narrative. The difference between
-- ($.)/delta and ($..)/delta_ is that ($..)/delta_ will
-- rethrow exceptions into the parent narrative via Fail
-- rather than calling Control.Exception.throw.
--
-- >    (resultObj,result) <- obj $. narrative
($..)
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super
       )
    => Object traits (Narrative self super)
    -> Narrative messages (Narrative self super) result
    -> Narrative self super (Object traits (Narrative self super),result)
($..) = __delta
{-# INLINE [2] ($..) #-}




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

    go (Super m) = m >>= go

    go (Return result) = pure (object,result)
{-# NOINLINE [2] _delta #-}


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
                return (obj,result)

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


__delta
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super
       )
    => Object traits (Narrative self super)
    -> Narrative messages (Narrative self super) result
    -> Narrative self super (Object traits (Narrative self super),result)
__delta object = go
  where
    go (Say symbol k) =
        let !(method,b) = ma (,) (deconstruct object) symbol
        in do !object' <- method object
              __delta object' (k b)

    go (Fail e) =
        Core.throw e

    go (Super m) =
        m >>= go

    go (Return result) = pure (object,result)
{-# NOINLINE [2] __delta #-}

{-# RULES

    "__delta obj (Fail e)"
        forall obj e.
            __delta obj (Fail e) =
                Core.throw e

    ;

    "__delta obj (Super m)"
        forall obj m.
            __delta obj (Super m) =
                m >>= __delta obj

    ;

    "__delta obj (Return result)"
        forall obj result.
            __delta obj (Return result) =
                pure (obj,result)

    ;

    "__delta obj (Say symbol k)"
        forall obj symbol k.
            __delta obj (Say symbol k) =
                let
                    (method,b) =
                        ma (,) (deconstruct obj) symbol

                in
                    do
                        object' <- method obj
                        __delta object' (k b)

  #-}

infixr 5 $!.
-- | Strict version of ($.)
($!.)
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super
       , NFData (Traits traits (Object traits super -> super (Object traits super)))
       )
    => Object traits super
    -> Narrative messages super result
    -> super (Object traits super,result)
($!.) obj = go
  where
    go (Fail e) = Exception.throw e

    go (Super sup) = do
        narrative <- sup
        go narrative

    go (Return result) = return (obj,result)

    go (Say message k) = do
        (obj',v) <- obj $. Say message Return
        obj' `deepseq` (obj' $!. k v)
{-# NOINLINE [2] ($!.) #-}

{-# RULES

    "($!.) obj (Fail e)"
        forall obj e.
            ($!.) obj (Fail e) =
                Exception.throw e

    ;

    "($!.) obj (Super m)"
        forall obj m.
            ($!.) obj (Super m) =
                m >>= ($!.) obj

    ;

    "($!.) obj (Return result)"
        forall obj result.
            ($!.) obj (Return result) =
                pure (obj,result)

    ;

    "($!.) obj (Say symbol k)"
        forall obj symbol k.
            ($!.) obj (Say symbol k) =
                let
                    (method,b) =
                        ma (,) (deconstruct obj) symbol

                in
                    do
                        object' <- method obj
                        ($!.) object' (k b)

  #-}

infixr 5 $!..
-- | Strict version of ($..)
($!..)
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super
       , NFData (Traits traits (Object traits (Narrative self super) -> Narrative self super (Object traits (Narrative self super))))
       )
    => Object traits (Narrative self super)
    -> Narrative messages (Narrative self super) result
    -> Narrative self super (Object traits (Narrative self super),result)
($!..) obj = go
  where
    go (Fail e) = Core.throw e

    go (Super sup) = do
        narrative <- sup
        go narrative

    go (Return result) = return (obj,result)

    go (Say message k) = do
        (obj',v) <- obj $. Say message Return
        obj' `deepseq` (obj' $!.. k v)
{-# NOINLINE [2] ($!..) #-}

{-# RULES

    "($!..) obj (Fail e)"
        forall obj e.
            ($!..) obj (Fail e) =
                Core.throw e

    ;

    "($!..) obj (Super m)"
        forall obj m.
            ($!..) obj (Super m) =
                m >>= ($!..) obj

    ;

    "($!..) obj (Return result)"
        forall obj result.
            ($!..) obj (Return result) =
                pure (obj,result)

    ;

    "($!..) obj (Say symbol k)"
        forall obj symbol k.
            ($!..) obj (Say symbol k) =
                let
                    (method,b) =
                        ma (,) (deconstruct obj) symbol

                in
                    do
                        object' <- method obj
                        ($!..) object' (k b)

  #-}

