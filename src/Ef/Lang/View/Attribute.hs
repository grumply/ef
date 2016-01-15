{-# LANGUAGE GADTs #-}
module Ef.Lang.View.Attribute
    ( Views(..)
    ) where



data Views r k
  where

    Views
        :: r
        -> k
        -> Views r k
