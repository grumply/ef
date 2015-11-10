{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Lang.Scoped.Or
    ( Or(..), isThis, isThat
    , EitherOr(..), eitherOr
    ) where

import Mop.Core

import Lang.Scoped.Exit

data a `Or` b = This a | That b

isThis :: a `Or` b -> Bool
isThis (This a) = True
isThis _ = False

isThat :: a `Or` b -> Bool
isThat (That b) = True
isThat _ = False

data EitherOr a b fs m = EitherOr
  { this :: a -> Pattern fs m (a `Or` b)
  , that :: b -> Pattern fs m (a `Or` b)
  }

eitherOr :: Is Exiting fs m => (EitherOr a b fs m -> Pattern fs m (a `Or` b)) -> Pattern fs m (a `Or` b)
eitherOr f = exits $ \exit -> f EitherOr
  { this = \a -> exit (This a)
  , that = \b -> exit (That b)
  }
