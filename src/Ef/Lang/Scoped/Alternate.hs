{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Scoped.Alternate
  ( Alternating
  , Alternate(..)
  , alternates
  , Alternatable
  , alternator
  ) where

import Ef.Core
import Ef.Data.Queue

import Unsafe.Coerce



-- | Symbol

data Alternating k

    = forall fs m a. Fork Int (Pattern fs m a)

    | forall fs m a. Atomically Int (Pattern fs m a)

    | Stop Int

    | FreshScope (Int -> k)



-- | Symbol Module

data Alternate fs m =
    Alternate
        { alt
              :: Pattern fs m ()
              -> Pattern fs m ()

        , atomically
              :: forall b.
                 Pattern fs m b
              -> Pattern fs m b
        }



-- | Attribute

data Alternatable k =
    Alternatable Int k



-- | Attribute Construct

alternator
    :: Uses Alternatable gs m
    => Attribute Alternatable gs m
alternator =
    Alternatable 0 $ \fs ->
        let
          Alternatable scope k =
              view fs

          newScope =
              succ scope

        in
          newScope `seq` return $ fs .=
             Alternatable newScope k



-- | Symbol/Attribute Symmetry

instance Symmetry Alternatable Alternating where
  symmetry use (Alternatable i k) (FreshScope ik) =
      use k (ik i)



-- | Local Scoping Construct + Substitution

alternates
    :: forall fs m a. Is Alternating fs m
    => (    Alternate fs m
         -> Pattern fs m a
       )
    -> Pattern fs m a
alternates f =
  do
    scope <- self (FreshScope id)
    let
      alternate =
          Alternate
              { alt =
                    \p ->
                        self $ Fork scope $
                            do
                              p
                              self (Stop scope)
              , atomically =
                    \p ->
                        self (Atomically scope p)
              }

    start scope (f alternate)



start
    :: (Is Alternating fs m)
    => Int
    -> Pattern fs m a
    -> Pattern fs m a
start _ (Pure r) =
    return r

start scope (M m) =
    let
      continue = start scope
    in
      M (fmap continue m)

start scope (Step sym bp) =
    let
      ignore =
          Step sym (start scope . bp)

      check i scoped =
          if i == scope then
              scoped
          else
              ignore
    in
      case prj sym of

          Just x ->
              case x of

                  Fork i child ->
                      check i $
                          let
                            result =
                                unsafeCoerce ()

                            continue =
                                bp result

                            newRunQueue =
                              enqueue (unsafeCoerce child) emptyQueue
                          in
                            rooted scope newRunQueue continue

                  Atomically i atom ->
                      check i $
                          let
                            contained =
                                unsafeCoerce atom
                          in
                            do
                              b <- start scope contained
                              let
                                result =
                                    unsafeCoerce b
                                continue =
                                    bp result
                              start scope continue

                  Stop i ->
                      check i $
                          return (unsafeCoerce ())

                  _ ->
                      ignore

          Nothing ->
              ignore



rooted
    :: forall fs m a.
       (Is Alternating fs m)
    => Int
    -> Queue (Pattern fs m a)
    -> Pattern fs m a
    -> Pattern fs m a
rooted scope rest (Pure r) =
    case dequeue rest of

      Nothing ->
          return r

      Just (rest',nxt) ->
          do
            _ <- rooted scope rest' nxt
            return r

rooted scope rest (M m) =
    M $ do
          p' <- m
          handleQueue p'
  where

    handleQueue
        :: Pattern fs m a
        -> m (Pattern fs m a)
    handleQueue continue =
        case dequeue rest of

            Nothing ->
                return (start scope continue)

            Just (newRest,next) ->
                let
                  newRunQueue =
                      enqueue (unsafeCoerce continue) newRest
                in
                  newRunQueue `seq` return $
                      rooted scope newRunQueue next

rooted scope rest (Step sym continue) =
    let
      ignore =
          Step sym (rooted scope rest . continue)

      check i scoped =
          if i == scope then
              scoped
          else
              ignore
    in
      case prj sym of

        Just x ->
          case x of

            Fork i child ->
                let
                  newRunQueue =
                      enqueue (unsafeCoerce child) rest

                  newContinue =
                      continue (unsafeCoerce ())
                in
                  check i $
                      rooted scope newRunQueue newContinue

            Atomically i atom ->
              let
                  newContinue value =
                      continue (unsafeCoerce value)

                  newRunQueue newRest value =
                      enqueue (newContinue value) newRest

                in
                  check i $
                    do
                      b <- start scope (unsafeCoerce atom)
                      case dequeue rest of

                          Nothing ->
                              start scope (continue b)

                          Just (newRest,nxt) ->
                              rooted scope (newRunQueue newRest b) nxt

            Stop i ->
                check i $
                    case dequeue rest of

                        Nothing ->
                            return (unsafeCoerce ())

                        Just (newRest,next) ->
                            rooted scope newRest next

            _ ->
                ignore

        Nothing ->

            Step sym $ \result ->
                case dequeue rest of

                  Nothing ->
                      start scope (continue result)

                  Just (newRest,nxt) ->

                      Step sym $ \newResult ->
                          let
                            newRunQueue =
                                enqueue (continue newResult) newRest
                          in
                            rooted scope newRunQueue nxt



-- | Inlines

{-# INLINE alternator #-}
{-# INLINE alternates #-}
