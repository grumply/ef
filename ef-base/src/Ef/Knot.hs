{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Knot (module Ef.Knot.Messages) where

import Ef.Ma

import Ef.Knot.Messages
import qualified Ef.Knot.Methods as Methods

instance Ma Methods.Knot Knot where
    ma use (Methods.Knot i k) (FreshScope ik) = use k (ik i)
