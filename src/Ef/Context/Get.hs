{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Context.Get
    ( module Ef.Context.Get.Lexemes
    ) where


import Ef.Core.Inflect

import Ef.Context.Get.Lexemes
import Ef.Context.Get.Context

import Unsafe.Coerce


instance Inflection Gets Get
  where

    inflect use (Gets _ _ k) (Reify k') =
        use k k'

    inflect use (Gets (o,k) _ _) (Get ok) =
        use k (ok (unsafeCoerce o))

    inflect use (Gets _ k _) (Reset k') =
        use k k'

