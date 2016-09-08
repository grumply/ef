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
    , ($.)
    , module Control.Monad.Catch
    , module Control.Monad.IO.Class
    , module Control.Monad.Trans.Class
    ) where

import Ef.Type.Set as Core (type (/==),Union)

import Ef.Type.Nat as Core (Offset)

import Ef.Object as Core
import Ef.Traits as Core

import Ef.Narrative as Core
import Ef.Messages as Core

import Ef.Machine as Core

import Ef.Codensity as Core

import Ef.Ma as Core

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Send a narrative to an object for invocation; returns a new, modified object
-- and a result.
--
-- >    (resultObj,result) <- delta obj narrative
delta
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super, MonadThrow super
       )
    => Object traits super
    -> Narrative messages super result
    -> super (Object traits super,result)
delta = _delta
{-# INLINE [2] delta #-}


infixr 5 $.
-- | Synonym for 'delta'. Send a narrative to an object.
--
-- >    (resultObj,result) <- obj $. narrative
($.)
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super, MonadThrow super
       )
    => Object traits super
    -> Narrative messages super result
    -> super (Object traits super,result)
($.) = _delta
{-# INLINE [2] ($.) #-}



_delta
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super, MonadThrow super
       )
    => Object traits super
    -> Narrative messages super result
    -> super (Object traits super,result)
_delta object = go
  where
    go (Say symbol k) =
        let ~(method,b) = ma (,) (deconstruct object) symbol
        in do object' <- method object
              _delta object' (k b)

    go (Fail e) =
        throwM e

    go (Super m) = m >>= go

    go (Return result) = pure (object,result)
{-# NOINLINE [2] _delta #-}


{-# RULES

    "_delta obj (Fail e)"
        forall obj e.
            _delta obj (Fail e) =
                throwM e

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
