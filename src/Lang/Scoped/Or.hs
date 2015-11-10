{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Lang.Scoped.Or
    ( Or(..), this, that
    , EitherOr(..), eitherOr
    ) where

import Mop.Core

import Lang.Scoped.Exit

import Prelude hiding (either,or)

-- Note that the overlap of this module's either with Prelude's is a failure
-- of Haskell, not this library. A future, better, version of Haskell should
-- allow this overlap. As it stands, this module is clunky and Exit should
-- be preferred, sadly.

data a `Or` b = This a | That b

this :: a `Or` b -> Bool
this (This a) = True
this _ = False

that :: a `Or` b -> Bool
that (That b) = True
that _ = False

data EitherOr a b fs m = EitherOr
  { either :: a -> Pattern fs m (a `Or` b)
  , or :: b -> Pattern fs m (a `Or` b)
  }

eitherOr :: Is Exiting fs m => (EitherOr a b fs m -> Pattern fs m (a `Or` b)) -> Pattern fs m (a `Or` b)
eitherOr f = exits $ \exit -> f EitherOr
  { either = \a -> exit (This a)
  , or = \b -> exit (That b)
  }
