{-# LANGUAGE GADTs #-}
module Ef.Lang.Set.Attribute
    ( Sets(..)
    ) where



import Ef.Core.Object



data Sets k
    where

        Sets
            :: (    Object contexts environment
                 -> k
               )
            -> Sets k
