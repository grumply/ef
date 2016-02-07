{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Set
    ( module Ef.Set.Messages
    ) where



import Ef.Ma

import Ef.Set.Messages
import qualified Ef.Set.Methods as Methods

import Unsafe.Coerce


instance Ma Methods.Set Set where
    ma use (Methods.Set ok) (Set o k) =
        let obj = unsafeCoerce o
        in use (ok obj) k
