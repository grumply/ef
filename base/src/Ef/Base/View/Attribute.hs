{-# LANGUAGE GADTs #-}
module Ef.Context.View.Attribute
    ( Views(..)
    ) where



data Views r k
  where

    Views
        :: r
        -> k
        -> Views r k
