{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Get
    ( Attr.get
    , Lex.introspect
    , Lex.Get
    )
    where



import Ef.Core

import qualified Ef.Lang.Get.Attribute as Attr
import qualified Ef.Lang.Get.Lexicon as Lex

import Unsafe.Coerce


instance Witnessing Attr.Getter Lex.Get
  where

    witness use (Getter _ _ k) (Reify k') =
        use k k'

    witness use (Getter (o,k) _ _) (Get ok) =
        use k (ok (unsafeCoerce o))

    witness use (Getter _ k _) (Reset k') =
        use k k'

