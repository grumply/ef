{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Ef.State.Methods
    ( State(..)
    , state
    ) where


import Ef.Object
import Control.DeepSeq

data State st k =
    State st (st -> k)
instance NFData st => NFData (State st k) where
    rnf (State st stk) = rnf st `seq` ()

state :: state -> Use (State state) lexicon environment
state initialState =
    State initialState $ \newState fs ->
        let !new = state newState
        in pure $ fs .= new


{-# INLINE state #-}
