{-# LANGUAGE GADTs #-}
module Ef.Context.State.Lexicon
    ( State(..)
    ) where



import           Ef.Core.Narrative



data State st k
    where

        Modify
            :: (st -> st)
            -> (st -> k)
            -> State st k
