{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Get
    ( module Ef.Lang.Get.Lexemes
    ) where


import Ef.Core.Inflect

import Ef.Lang.Get.Lexemes
import Ef.Lang.Get.Context

import Unsafe.Coerce


instance Inflection Attribute Lexicon
  where

    inflect use (Getter _ _ k) (Reify k') =
        use k k'

    inflect use (Getter (o,k) _ _) (Get ok) =
        use k (ok (unsafeCoerce o))

    inflect use (Getter _ k _) (Reset k') =
        use k k'

