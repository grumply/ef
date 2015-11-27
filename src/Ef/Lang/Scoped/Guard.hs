{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.Guard
    ( Guarding
    , guards
    , Guardable
    , guarder
    , Guard(..)
    ) where



import Ef.Core

import Data.Binary
import Data.Foldable
import Unsafe.Coerce



-- | Symbol

data Guarding k
  where

    FreshScope
        :: (Int -> k)
        -> Guarding k

    Choose
        :: Int
        -> [a]
        -> (a -> k)
        -> Guarding k

    Cut
        :: Int
        -> Guarding k



-- | Symbol Module

data Guard fs m =
    Guard
        { choose
              :: forall f a.
                 Foldable f
              => f a -> Pattern fs m a

        , cut
              :: forall b.
                 Pattern fs m b
        }



-- | Attribute

data Guardable k
  where

    Guardable
        :: Int
        -> k
        -> Guardable k


instance Uses Guardable gs m
    => Binary (Attribute Guardable gs m)
  where

    get =
        do
          scope <- get
          let
            Guardable _ k = guarder

          return (Guardable scope k)

    put (Guardable scope _) =
        put scope



-- | Attribute Construct

guarder
    :: Uses Guardable fs m
    => Attribute Guardable fs m
guarder =
    Guardable 0 $ \fs ->
        let
          Guardable i k =
              view fs

        in
          return $ fs .=
              Guardable (succ i) k



-- | Symbol/Attribute pairing witness

instance Witnessing Guardable Guarding
  where

    witness use (Guardable i k) (FreshScope ik) =
        use k (ik i)



-- | Local Scoping Construct

guards
    :: forall fs m a.
       Is Guarding fs m
    => (    Guard fs m
         -> Pattern fs m a
       )
    -> Pattern fs m (Maybe a)
guards l =
  do
    scope <- self (FreshScope id)
    rewrite scope $ l
        Guard
            { choose =
                  \foldable ->
                      let
                        list = toList foldable

                      in
                        self (Choose scope list id)

            , cut =
                  self (Cut scope)
            }



rewrite
    :: Is Guarding fs m
    => Int
    -> Pattern fs m a
    -> Pattern fs m (Maybe a)

rewrite _ (Fail e) =
    Fail e

rewrite _ (Pure result) =
    Pure (Just result)

rewrite scope (M m) =
    let
      continue = rewrite scope

    in
      M (fmap continue m)

rewrite scope (Step sym bp) =
    let
      ignore =
          Step sym (rewrite scope . bp)

      check i scoped =
          if i == scope then
              scoped
          else
              ignore
    in
      case prj sym of

          Just x ->
              case x of

                  Choose i as _ ->
                      check i $
                          choosing
                              scope
                              as
                              (unsafeCoerce bp)
                              (return Nothing)

                  _ ->
                      ignore

          Nothing ->
              Step sym (rewrite scope . bp)



choosing
    :: Is Guarding fs m
    => Int
    -> [a]
    -> (a -> Pattern fs m r)
    -> Pattern fs m r
    -> Pattern fs m r
choosing _ [] _ alt =
    alt

choosing scope (a:as) bp alt =
    nestedChoosing scope as alt bp (bp a)



nestedChoosing
    :: Is Guarding fs m
    => Int
    -> [a]
    -> Pattern fs m r
    -> (a -> Pattern fs m r)
    -> Pattern fs m r
    -> Pattern fs m r
nestedChoosing _ _ _ _ (Pure result) =
    return result

nestedChoosing scope choices alt parentContinue (M m) =
    let
      continue = nestedChoosing scope choices alt parentContinue

    in
      M (fmap continue m)

nestedChoosing scope choices alt parentContinue (Step sym childContinue) =
    let
      ignore =
          let
            continue =
                nestedChoosing scope choices alt parentContinue . childContinue

          in
            Step sym continue

      check i scoped =
          if i == scope then
              scoped
          else
              ignore

    in
      case prj sym of

          Just x ->
              case x of

                  Choose i nestedChoices _ ->
                      check i $
                          choosing
                              scope
                              (unsafeCoerce nestedChoices)
                              (unsafeCoerce childContinue)
                              (choosing scope choices parentContinue alt)

                  Cut i ->
                      check i $
                          choosing scope choices parentContinue alt

                  _ ->
                      ignore

          Nothing ->
              ignore



-- | Inlines

{-# INLINE guarder #-}
{-# INLINE guards #-}
