{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Vary.Context
    ( Varies(..)
    , varies
    ) where



import Ef.Core.Object
import Ef.Lang.Vary.Attribute

import qualified Data.Binary as B



instance ( Has (Varies st) contexts environment
         , B.Binary st
         )
    => B.Binary (Varies st (Morphism contexts environment))
  where

    get =
        do
          st <- B.get
          return (varies st)

    put (Varies st _) =
        B.put st



varies
    :: state
    -> Use (Varies state) lexicon environment

varies initialState =
    Varies initialState $ \newState fs ->
        pure $ fs .=
            varies newState
