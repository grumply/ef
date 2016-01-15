{-# LANGUAGE GADTs #-}
module Ef.Lang.Vary.Attribute
    ( Varies(..)
    ) where



data Varies st k
    where

        Varies
            :: st
            -> (st -> k)
            -> Varies st k
