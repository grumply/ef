{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Knot.Methods (Knot(..), knots) where

import Ef.Object

data Knot k = Knot Int k

knots =
    Knot 0 $ \fs ->
        let Knot n k = view fs
            n' = succ n
        in n' `seq` pure $ fs .= Knot n' k
{-# INLINE knots #-}
