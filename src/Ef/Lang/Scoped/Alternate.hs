{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Scoped.Alternate
    ( Alternating
    , alternates

    , Alternate(..)

    , Alternatable
    , alternator
    ) where

import Ef.Core
import Ef.Data.Queue

import Unsafe.Coerce



-- | Symbol

data Alternating k =

      forall fs m a. Fork Int (Pattern fs m a)

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



-- | Symbol/Attribute pairing witness

instance Witnessing Alternatable Alternating where
    witness use (Alternatable i k) (FreshScope ik) =
        use k (ik i)



-- | Local Scoping Construct + Substitution

alternates
    :: Is Alternating fs m
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

    rewrite scope (f alternate)



rewrite
    :: Is Alternating fs m
    => Int
    -> Pattern fs m a
    -> Pattern fs m a
rewrite _ (Pure r) =
    return r

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

                  Fork i child ->
                      check i $
                          let
                            result =
                                unsafeCoerce ()

                            newRunQueue =
                              enqueue (unsafeCoerce child) emptyQueue

                          in
                            rooted scope newRunQueue (bp result)

                  Atomically i atom ->
                      check i $
                          do
                            b <- rewrite scope (unsafeCoerce atom)
                            let
                              continue =
                                  bp (unsafeCoerce b)

                            rewrite scope continue

                  Stop i ->
                      check i $
                          return (unsafeCoerce ())

                  _ ->
                      ignore

          Nothing ->
              ignore



rooted
    :: Is Alternating fs m
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

    handleQueue continue =
        case dequeue rest of

            Nothing ->
                return (rewrite scope continue)

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
               check i $
                   let
                     newRunQueue =
                         enqueue (unsafeCoerce child) rest

                     newContinue =
                         continue (unsafeCoerce ())

                   in
                     rooted scope newRunQueue newContinue

            Atomically i atom ->
                check i $
                    do
                      let
                        newContinue value =
                            continue (unsafeCoerce value)

                        newRunQueue newRest value =
                            enqueue (newContinue value) newRest

                      b <- rewrite scope (unsafeCoerce atom)
                      case dequeue rest of

                          Nothing ->
                              rewrite scope (continue b)

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
            let
              newContinue value =
                  case dequeue rest of

                      Nothing ->
                          rewrite scope (continue value)

                      Just (newRest,nxt) ->
                          let
                            newRunQueue =
                                enqueue (continue value) newRest

                          in
                            rooted scope newRunQueue nxt

            in
              Step sym newContinue



-- | Inlines

{-# INLINE alternator #-}
{-# INLINE alternates #-}
