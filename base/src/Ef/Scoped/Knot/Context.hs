{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef.Lang.Knot.Context
    ( Knot(..)
    , knots
    ) where



import Ef.Lang.Knot.Attribute
import Ef.Core.Object

import Data.Binary


instance (Has Knot contexts environment)
    => Binary (Knot (Morphism contexts environment))
  where

    get =
        return knots



    put _ =
        pure ()



knots
    :: Use Knot contexts environment

knots =
    Knot 0 $ \fs ->
        let
          Knot n k =
              view fs

          n' =
              succ n

        in
          n' `seq` pure $ fs .=
              Knot n' k



{-# INLINE knots #-}
