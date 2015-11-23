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

import Data.IORef
import Unsafe.Coerce



-- | Symbol

data Operation status result
  where

    Operation
        :: Int
        -> IORef (Either status result)
        -> Operation status result



data Await
  where

    Atomically
        :: Await

    Concurrently
        :: Await



data Alternating k
  where

    FreshScope
        :: (Int -> k)
        -> Alternating k

    Fork
        :: Int
        -> Pattern fs m result
        -> (Operation status result -> k)
        -> Alternating k

    Await
        :: Int
        -> Await
        -> Operation status result
        -> (result -> k)
        -> Alternating k

    Atomic
        :: Int
        -> Pattern fs m result
        -> Alternating k

    Stop
        :: Int
        -> Operation status result
        -> (result -> k)
        -> Alternating k

    Update
        :: Int
        -> Operation status result
        -> status
        -> Alternating k

    Status
        :: Int
        -> Operation status result
        -> (status -> k)
        -> Altrnating k

    Finished
        :: Operation a
        -> (Bool -> k)
        -> Alternating k



-- | Symbol Module

data Alternate fs m =
    Alternate
        { alt
              :: Pattern fs m a
              -> Pattern fs m (Operation a)

        ,

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
      rewrite scope emptyQueue $ f
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




rewrite
    :: Is Alternating fs m
    => Int
    -> Queue (Pattern fs m a)
    -> Pattern fs m a
    -> Pattern fs m a
rewrite scope =
    withQueue
  where

    withQueue queue =
        go
      where

        -- Returned in root since alternates end in Stop.
        go (Pure r) =
            case dequeue queue of

                -- All forks completed; return.
                Nothing ->
                    return r

                -- Finish running forks and then return.
                Just (newQueue,next) ->
                    do
                      _ <- withQueue newQueue next
                      return r

        -- Monadic actions are alternated the same as Steps.
        go (M m) =
            case dequeue queue of

                Nothing ->
                    M (fmap go m)

                Just (newQueue,next) ->
                    let

                      newRunQueue continue =
                          enqueue (unsafeCoerce continue) newQueue

                    in
                      do
                        continue <- m
                        withQueue (newRunQueue continue) next

        go (Step sym bp) =
            let
              ignore =
                  Step sym (go . bp)

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

                                    newQueue =
                                        enqueue (unsafeCoerce child) queue

                                    continue =
                                        bp result

                                  in
                                    withQueue newQueue continue

                          Atomically i atom ->
                              check i $
                                  do
                                    b <- unsafeCoerce atom
                                    let
                                      continue b =
                                          bp b

                                    go continue

                          Stop i ->
                              check i $
                                  case

                          _ ->
                              ignore

                  Nothing ->
                      ignore



-- | Inlines

{-# INLINE rooted #-}
{-# INLINE rewrite #-}
{-# INLINE alternator #-}
{-# INLINE alternates #-}
