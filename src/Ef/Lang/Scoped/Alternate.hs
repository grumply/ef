{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
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
        {
          alt
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

    substitute scope (f alternate)
  where

    substitute scope = start
      where

        start p =
            case p of

            Step sym bp ->
              let
                ignore =
                    Step sym (start . bp)

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
                            continue =
                              bp (unsafeCoerce ())

                            newRunQueue =
                              enqueue (unsafeCoerce child) emptyQueue
                          in
                            check i $
                                rooted newRunQueue continue

                      Atomically i atom ->
                        check i $
                            do
                              b <- start (unsafeCoerce atom)
                              start (bp (unsafeCoerce b))

                      Stop i ->
                        check i $
                            return (unsafeCoerce ())

                      _ ->
                          ignore

                  Nothing ->
                      ignore

            M m ->
                M (fmap start m)

            Pure r ->
                return r

        rooted rest p =
            case p of

                Step sym bp ->
                    let
                      ignore =
                          Step sym (rooted rest . bp)

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

                                  continue =
                                      bp (unsafeCoerce ())
                                in
                                  check i $
                                      rooted newRunQueue continue

                            Atomically i atom ->
                                let
                                  newRunQueue rest' b =
                                      enqueue (continue b) rest'

                                  continue b =
                                      bp (unsafeCoerce b)
                                in
                                  check i $
                                    do
                                      b <- start (unsafeCoerce atom)
                                      case dequeue rest of

                                          Nothing ->
                                              start (continue b)

                                          Just (rest',nxt) ->
                                              rooted (newRunQueue rest' b) nxt

                            Stop i ->
                                check i $
                                    case dequeue rest of

                                        Nothing ->
                                            Pure (unsafeCoerce ())

                                        Just (rest',next) ->
                                            rooted rest' next

                            _ ->
                                ignore

                        Nothing ->

                            Step sym $ \b ->
                                case dequeue rest of

                                  Nothing ->
                                      start (bp b)

                                  Just (rest',nxt) ->

                                      Step sym $ \b' ->
                                          let
                                            newRunQueue =
                                                enqueue (bp b') rest'
                                          in
                                            rooted newRunQueue nxt

                M m ->
                    M $ do
                          p' <- m
                          case dequeue rest of

                              Nothing ->
                                  return (start p')

                              Just (rest',next) ->
                                  let
                                    newRunQueue =
                                        enqueue (unsafeCoerce p') rest'
                                  in
                                    newRunQueue `seq` return $
                                        rooted newRunQueue next

                Pure r ->
                  case dequeue rest of

                    Nothing ->
                        return r

                    Just (rest',nxt) ->
                        do
                          rooted rest' nxt
                          return r

-- | Inlines

{-# INLINE alternator #-}
{-# INLINE alternates #-}
