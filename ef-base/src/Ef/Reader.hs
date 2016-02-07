{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Reader
    ( module Ef.Reader.Messages
    ) where



import Ef.Ma

import Ef.Reader.Messages
import qualified Ef.Reader.Methods as Methods


instance Ma (Methods.Reader r) (Reader r) where
    ma use (Methods.Reader r k) (Ask rk) = ma use (r,k) rk
