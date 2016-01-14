{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Ef.Lang.Note.Attribute
    ( Attribute(..)
    , Notes
    ) where



import Ef.Core.Object



data Attribute r k
    where

        Noter
            :: r
            -> (r -> k)
            -> Attribute r k



type Notes r contexts environment =
    Has (Attribute r) contexts environment
