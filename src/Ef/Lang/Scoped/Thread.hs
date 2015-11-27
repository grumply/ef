{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.Thread
    ( Threading
    , threads
    , Threadable
    , threader
    , Thread(..)
    , rewrite
    ) where



import Ef.Core
import Ef.Data.Queue

import Data.Binary
import Unsafe.Coerce



data Threading k
  where

    Fork
        :: Int
        -> Pattern fs m a
        -> k
        -> Threading k

    Yield
        :: Int
        -> k
        -> Threading k

    Stop
        :: Int
        -> Threading k

    FreshScope
        :: (Int -> k)
        -> Threading k



data Thread fs m =
    Thread
        {
          fork
              :: Pattern fs m ()
              -> Pattern fs m ()

        , yield
              :: Pattern fs m ()
        }



data Threadable k
  where

    Threadable
        :: Int
        -> k
        -> Threadable k



instance Uses Threadable gs m
    => Binary (Attribute Threadable gs m)
  where

    get =
        return threader



    put _ =
        pure ()



threader
    :: Uses Threadable gs m
    => Attribute Threadable gs m

threader = Threadable 0 $ \fs ->
    let
      Threadable i k =
          view fs

      i' =
          succ i

    in
      i' `seq` pure $ fs .=
          Threadable i' k



-- | Attribute/Symbol Symmetry

instance Threadable `Witnessing` Threading
  where

    witness use (Threadable i k) (FreshScope ik) =
        use k (ik i)



-- | Local Scoping Construct + Substitution

threads
    :: Is Threading fs m
    => (    Thread fs m
         -> Pattern fs m a
       )
    -> Pattern fs m a

threads f =
    do
      scope <- self (FreshScope id)
      rewrite scope emptyQueue $ f
          Thread
              {
                fork =
                    \p ->
                        let
                          child =
                              p >> self (Stop scope)

                        in
                          self (Fork scope child ())

              , yield =
                    self (Yield scope ())
              }



-- This type signature decreases performance! Why? Look at core.
-- rewrite
--     :: forall (t :: [* -> *]) (m :: * -> *) a.
--        (Monad m, Allows' Threading t (IndexOf Threading t))
--     => Int
--     -> Queue (Pattern t m a)
--     -> Pattern t m a
--     -> Pattern t m a

rewrite scope =
    withQueue
  where

    withQueue queue =
        go
      where

        go (Fail e) =
            Fail e

        go (M m) =
            M (fmap go m)

        go (Pure r) =
            case dequeue queue of

                Nothing ->
                    return r

                Just (rest,nxt) ->
                    let
                      newQ =
                          enqueue (return r) rest

                    in
                      newQueue `seq`
                          withQueue newQ (unsafeCoerce nxt)

        go (Step sym bp) =
            let
              check i scoped =
                  if i == scope then
                      scoped
                  else
                      ignore

              ignore =
                  Step sym (go . bp)

            in
              case prj sym of

                Just x ->
                    case x of

                        Fork i child k ->
                            check i $
                                let
                                  newQ =
                                      enqueue (unsafeCoerce child) queue

                                in
                                  newQ `seq`
                                      withQueue newQ (bp k)

                        Yield i k ->
                            check i $
                                case dequeue queue of

                                    Nothing ->
                                        go (bp k)

                                    Just (rest,nxt) ->
                                        let
                                          newQ =
                                              enqueue (bp k) rest

                                        in
                                          newQ `seq`
                                              withQueue newQ nxt

                        Stop i ->
                            check i $
                                case dequeue queue of

                                     ~(Just q) ->
                                         let
                                           (rest,nxt) =
                                               q

                                         in
                                           rewrite scope rest nxt

                Nothing ->
                    ignore



-- | Inlines

{-# INLINE rewrite #-}
{-# INLINE threader #-}
{-# INLINE threads #-}
