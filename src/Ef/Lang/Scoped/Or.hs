{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.Lang.Scoped.Or
    ( Or(..)
    , isThis
    , isThat

    , EitherOr(..)
    , eitherOr
    ) where



import Ef.Core

import Ef.Lang.Scoped.Exit



data a `Or` b
  where

    This
        :: a
        -> a `Or` b

    That
        :: b
        -> a `Or` b



over :: (a -> c) -> (b -> c) -> a `Or` b -> c
over f _ (This a) =
    f a

over _ g (That b) =
    g b



isThis
    :: a `Or` b
    -> Bool
isThis =
    over (const True) (const False)



isThat
    :: a `Or` b
    -> Bool
isThat =
    over (const False) (const True)



data EitherOr a b fs m =
    EitherOr
        { this
              :: a
              -> Pattern fs m (a `Or` b)
        , that
              :: b
              -> Pattern fs m (a `Or` b)
        }



eitherOr
    :: Is Exiting fs m
    => (    EitherOr a b fs m
         -> Pattern fs m (a `Or` b)
       )
    -> Pattern fs m (a `Or` b)
eitherOr f =
  exits $ \Exit{..} -> f
      EitherOr
          { this =
                \a ->
                    exit (This a)
          , that =
                \b ->
                    exit (That b)
          }
