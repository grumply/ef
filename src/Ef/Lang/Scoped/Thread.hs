{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
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



data Thread scope parent =
    Thread
        {
          fork
              :: Pattern scope parent ()
              -> Pattern scope parent ()

        , yield
              :: Pattern scope parent ()
        }



data Threadable k
  where

    Threadable
        :: Int
        -> k
        -> Threadable k



instance Uses Threadable attrs parent
    => Binary (Attribute Threadable attrs parent)
  where

    get =
        return threader



    put _ =
        pure ()



threader
    :: Uses Threadable attrs parent
    => Attribute Threadable attrs parent

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
    :: Is Threading scope parent
    => (    Thread scope parent
         -> Pattern scope parent a
       )
    -> Pattern scope parent a

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



-- This type signature decreases performance even though this is the derived type! Why? Need to look at core. 
-- Seems likely to be either a GHC bug or an optimization from an unsafeCoerce. Either way, tests don't seem to 
-- show a failure here.
-- rewrite
--     :: forall scope parent result.
--        Is Threading scope parent
--     => Int
--     -> Queue (Pattern scope parent result)
--     -> Pattern scope parent result
--     -> Pattern scope parent result

rewrite scope =
    withQueue
  where

    withQueue queue =
        go
      where

        go (Fail e) =
            Fail e

        go (Super m) =
            Super (fmap go m)

        go (Pure r) =
            case dequeue queue of

                Nothing ->
                    return r

                Just (nxt,rest) ->
                    let
                      newQ =
                          enqueue (return r) rest

                    in
                      newQueue `seq`
                          withQueue newQ (unsafeCoerce nxt)

        go (Send sym bp) =
            let
              check i scoped =
                  if i == scope then 
                      scoped
                  else
                      ignore

              ignore =
                  Send sym (go . bp)

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

                                    Just (nxt,rest) ->
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
                                           ~(nxt,rest) =
                                               q

                                         in
                                           rewrite scope rest nxt

                Nothing ->
                    ignore



-- | Inlines

{-# INLINE rewrite #-}
{-# INLINE threader #-}
{-# INLINE threads #-}
