{-# LANGUAGE GADTs #-}
module Ef.Context.State.Attribute
    ( Stateful(..)
    ) where



data Stateful st k
    where

        State
            :: !st
            -> (st -> k)
            -> Stateful st k
