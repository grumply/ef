{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef.Lang.Except.Context
    ( Excepts(..)
    , excepter
    ) where



import Ef.Core.Object
import Ef.Lang.Except.Attribute

import Data.Binary



instance Has Excepts contexts environment
    => Binary (Excepts (Morphism contexts environment))
  where

    get =
        pure excepter



    put _ =
        pure ()



excepter
    :: Use Excepts contexts environment

excepter =
    let
      uncaught err =
          "Impossible uncaught checked exception: " ++ (show err)

    in
      Excepts (error . uncaught)

{-# INLINE excepter #-}
