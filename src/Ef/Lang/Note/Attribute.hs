{-# LANGUAGE GADTs #-}
module Ef.Lang.Note.Attribute
    ( Notes(..)
    ) where



import Ef.Core.Object



data Notes r k
    where

        Notes
            :: r
            -> (r -> k)
            -> Notes r k
