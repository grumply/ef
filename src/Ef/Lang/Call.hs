module Ef.Lang.Call where



import GHC.StaticPtr



data Methoding k
  where

    Method
        :: StaticPtr (Pattern fs m a)
        -> (a -> k)
        -> Method



data Methodable k
  where

    Methodable
        :: k
        -> Methodable k


instance Methodable `Witnessing` Methoding
