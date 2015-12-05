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

data Guard scope parent =
    Guard
        { choose
              :: forall f a.
                 Foldable f
              => f a -> Pattern scope parent a

        , cut
              :: forall b.
                 Pattern scope parent b
        }



data Guardable k
  where

    Guardable
        :: Int
        -> k
        -> Guardable k



instance Uses Guardable attrs parent
    => Binary (Attribute Guardable attrs parent)
  where

    get =
        return guarder



    put _ =
        pure ()



-- | Attribute Construct

guarder
    :: Uses Guardable attrs parent
    => Attribute Guardable attrs parent

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
    :: forall scope parent result.
       Is Guarding scope parent
    => (    Guard scope parent
         -> Pattern scope parent result
       )
    -> Pattern scope parent (Maybe result)

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
    :: Is Guarding scope parent
    => Int
    -> Pattern scope parent result
    -> Pattern scope parent (Maybe result)

rewrite _ (Fail e) =
    Fail e

rewrite _ (Pure result) =
    Pure (Just result)

rewrite scope (Super m) =
    let
      continue = rewrite scope

    in
      Super (fmap continue m)

rewrite scope (Send sym bp) =
    let
      ignore =
          Send sym (rewrite scope . bp)

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
              Send sym (rewrite scope . bp)



choosing
    :: Is Guarding scope parent
    => Int
    -> [a]
    -> (a -> Pattern scope parent r)
    -> Pattern scope parent r
    -> Pattern scope parent r
choosing _ [] _ alt =
    alt

choosing scope (a:as) bp alt =
    nestedChoosing scope as alt bp (bp a)



nestedChoosing
    :: Is Guarding scope parent
    => Int
    -> [a]
    -> Pattern scope parent result
    -> (a -> Pattern scope parent result)
    -> Pattern scope parent result
    -> Pattern scope parent result

nestedChoosing _ _ _ _ (Pure result) =
    return result

nestedChoosing scope choices alt parentContinue (Super m) =
    let
      continue = nestedChoosing scope choices alt parentContinue

    in
      Super (fmap continue m)

nestedChoosing scope choices alt parentContinue (Send sym childContinue) =
    let
      ignore =
          let
            continue =
                nestedChoosing scope choices alt parentContinue . childContinue

          in
            Send sym continue

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
