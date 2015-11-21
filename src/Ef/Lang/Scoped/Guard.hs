{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Ef.Lang.Scoped.Guard
    ( Guarding
    , guards
    , Guardable
    , guarder
    , Guard(..)
    ) where

import Ef.Core

import Data.Foldable
import Unsafe.Coerce



-- | Symbol

data Guarding k

    = FreshScope (Int -> k)

    | forall a. Choose Int [a] (a -> k)

    | Cut Int



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

data Guardable k =
    Guardable Int k



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



-- | Symbol/Attribute Symmetry

instance Symmetry Guardable Guarding where
    symmetry use (Guardable i k) (FreshScope ik) =
        use k (ik i)



-- | Local Scoping Construct

guards
    :: forall fs m a.
       (Is Guarding fs m)
    => (    Guard fs m
         -> Pattern fs m a
       )
    -> Pattern fs m (Maybe a)
guards l =
  do
    scope <- self (FreshScope id)
    rewrite scope $ l
        Guard
            { choose = \foldable ->
                  let
                    list = toList foldable
                  in
                    self (Choose scope list id)

            , cut =
                  self (Cut scope)
            }



rewrite
    :: (Is Guarding fs m)
    => Int
    -> Pattern fs m a
    -> Pattern fs m (Maybe a)
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

rewrite scope (M m) =
    let
      continue = rewrite scope
    in
      M (fmap continue m)

rewrite _ (Pure result) =
    return (Just result)



choosing
    :: (Is Guarding fs m)
    => Int
    -> [a]
    -> (a -> Pattern fs m r)
    -> Pattern fs m r
    -> Pattern fs m r
choosing _ [] _ alt =
    alt

choosing scope (a:as) bp alt =
    nestedChoices scope as alt bp (bp a)



nestedChoices
    :: (Is Guarding fs m)
    => Int
    -> [a]
    -> Pattern fs m r
    -> (a -> Pattern fs m r)
    -> Pattern fs m r
    -> Pattern fs m r
nestedChoices scope as alt bp (Step sym bp') =
    let
      ignore =
          Step sym (nestedChoices scope as alt bp . bp')

      check i scoped =
          if i == scope then
              scoped
          else
              ignore
    in
      case prj sym of

          Just x ->
              case x of

                  Choose i as' _ ->
                      check i $
                          choosing
                              scope
                              (unsafeCoerce as')
                              (unsafeCoerce bp')
                              (choosing scope as bp alt)

                  Cut i ->
                      check i $
                          choosing scope as bp alt

                  _ ->
                      ignore

          Nothing ->
              ignore

nestedChoices scope as alt bp (M m) =
    let
      nextTry = nestedChoices scope as alt bp
    in
      M (fmap nextTry m)

nestedChoices _ _ _ _ (Pure result) =
    return result

-- | Inlines

{-# INLINE guarder #-}
{-# INLINE guards #-}
