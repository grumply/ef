{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Context.State.Context
    ( Stateful(..)
    , state
    ) where



import Ef.Core.Object
import Ef.Context.State.Attribute

import qualified Data.Binary as B



instance ( Has (Stateful st) contexts environment
         , B.Binary st
         )
    => B.Binary (Stateful st (Morphism contexts environment))
  where

    get =
        do
          st <- B.get
          return (state st)

    put (State st _) =
        B.put st



state
    :: state
    -> Use (Stateful state) lexicon environment

state initialState =
    State initialState $ \newState fs ->
        pure $ fs .=
            state newState



{-# INLINE state #-}
