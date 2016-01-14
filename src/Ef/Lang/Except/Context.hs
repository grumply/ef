{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef.Lang.Except.Context
    ( Attribute(..)
    , excepter
    ) where



import Ef.Core.Object
import Ef.Lang.Except.Attribute

import Data.Binary



instance Has Attribute contexts environment
    => Binary (Attribute (Morphism contexts environment))
  where

    get =
        pure excepter



    put _ =
        pure ()



excepter
    :: Use Attribute contexts environment

excepter =
    let
      uncaught err =
          "Impossible uncaught checked exception: " ++ (show err)

    in
      Except (error . uncaught)

{-# INLINE excepter #-}
