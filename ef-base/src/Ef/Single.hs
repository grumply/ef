{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Single (module Ef.Knot.Messages.Single) where

import Ef.Ma

import Ef.Knot.Messages.Single
import qualified Ef.Knot.Methods.Single as Methods

instance Ma Methods.SingleKnot SingleKnot
