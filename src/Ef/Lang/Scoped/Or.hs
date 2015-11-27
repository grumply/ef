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

import Data.Bifunctor
import Data.Binary



data a `Or` b
  where

    This
        :: a
        -> a `Or` b

    That
        :: b
        -> a `Or` b

  deriving (Eq,Ord,Show,Read)


instance Functor (Or a)
   where

     fmap f (That b) =
         That (f b)

     fmap _ (This a) =
         This a



instance Bifunctor Or
  where

    bimap frst _ (This a) =
        This (frst a)

    bimap _ scnd (That b) =
        That (scnd b)



instance Applicative (Or a)
  where

    pure =
        That



    (That ab) <*> o =
        fmap ab o

    (This a) <*> _ =
        This a



instance Monad (Or a)
  where

    return =
        pure



    (This a) >>= _ =
        This a

    (That b) >>= f =
        f b



instance ( Binary a
         , Binary b
         )
    => Binary (a `Or` b)
  where

    get =
        do
          i <- getWord8
          case i of

              0 ->
                  This <$> get

              1 ->
                  That <$> get

    put (This a) =
        putWord8 0 >> put a

    put (That b) =
        putWord8 1 >> put b



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
