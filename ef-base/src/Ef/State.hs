{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.State
    ( module Ef.State.Messages
    ) where


import Ef.Ma

import Ef.State.Messages
import qualified Ef.State.Methods as Methods


instance Ma (Methods.State st) (State st) where
    ma use (Methods.State st stk) (Modify stst stk') =
        let st' = stst st
            k   = stk st'
            k'  = stk' st
        in use k k'
