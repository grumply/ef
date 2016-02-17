{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Knot.Methods.Single (SingleKnot(..), singleKnot) where

import Ef.Object

data SingleKnot k = SingleKnot k

singleKnot =
    SingleKnot return
{-# INLINE singleKnot #-}
