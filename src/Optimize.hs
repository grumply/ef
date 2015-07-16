module Optimize where

class Optimize a where
  optm :: a -> a
  optm = id
