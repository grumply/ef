{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Knot (module Ef.Knot.Messages) where

import Ef.Ma

import Ef.Knot.Messages
import Ef.Knot.Methods

instance Ma Knot Knots where
    ma use (Knot i k) (FreshScope ik) = use k (ik i)
