{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Get
    ( module Ef.Get.Messages
    ) where


import Ef.Ma

import qualified Ef.Get.Methods as Methods
import Ef.Get.Messages

import Unsafe.Coerce


instance Ma Methods.Get Get
  where

    ma use (Methods.Get _ _ k) (Reify k') =
        use k k'

    ma use (Methods.Get (o,k) _ _) (Get ok) =
        use k (ok (unsafeCoerce o))

    ma use (Methods.Get _ k _) (Reset k') =
        use k k'

