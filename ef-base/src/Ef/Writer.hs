{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Writer
    ( module Ef.Writer.Messages
    ) where



import Ef.Ma

import Ef.Writer.Messages
import qualified Ef.Writer.Methods as Methods


instance Ma (Methods.Writer r) (Writer r) where
    ma use (Methods.Writer _ rk) (Tell r k) = ma use rk (r,k)
