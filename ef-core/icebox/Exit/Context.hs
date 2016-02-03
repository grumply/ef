{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef.Lang.Scoped.Exit.Context
    ( Exits(..)
    , exits
    ) where



import Ef.Core.Object
import Ef.Lang.Scoped.Exit.Attribute

import qualified Data.Binary as B



instance Has Exits context environment
    => B.Binary (Exits (Morphism context environment))
  where

    get =
        return exits



    put _ =
        pure ()



exits
    :: Use Exits contexts environment

exits =
    Exits 0 $ \fs ->
        let
          Exits i k =
              view fs

          i' =
              succ i

        in
          i' `seq` pure $ fs .=
              Exits i' k
