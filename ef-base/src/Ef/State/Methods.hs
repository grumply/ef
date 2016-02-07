{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Ef.State.Methods
    ( State(..)
    , state
    ) where


import Ef.Object


data State st k =
    State st (st -> k)


state :: state -> Use (State state) lexicon environment
state initialState =
    State initialState $ \newState fs ->
        let !new = state newState
        in pure $ fs .= new


{-# INLINE state #-}
