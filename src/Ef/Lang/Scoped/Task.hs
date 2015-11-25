{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Scoped.Task
    ( Tasking
    , alternates

    , Task(..)

    , Taskable
    , alternator
    ) where



import Ef.Core
import Ef.Data.Queue

import Data.IORef
import Unsafe.Coerce



-- | Symbol



data Level
  where

    Level
        :: Int
        -> Level



data Tier
  where

    Tier
        :: (Level,IORef (Queue Task))
        -> Tier



data Finalizers
  where

    Finalizers
        :: IORef [IO ()]
        -> Finalizers



data TVar a
  where

    TVar
        :: MVar a
        -> Finalizers
        -> TVar a



data Table
  where

    -- A table is a list of Tiers + an oustanding Tier
    -- to represent blocked computations. The outstanding
    -- tier prevents a subsystem from stopping when all
    -- Tiers in the [Tier] are empty.
    Table
        :: [Tier]
        -> Tier
        -> Table



data Subsystem
  where

    Subsystem
        :: Table
        -> Subsystem



data Priority
  where

    Highest
        :: Priority

    Higher
        :: Priority

    High
        :: Priority

    Normal
        :: Priority

    Low
        :: Priority

    Lower
        :: Priority

    Lowest
        :: Priority



data Chunking
  where

    NonChunked
        :: Chunking

    Chunked
        :: Int
        -> Chunking



data Task
  where

    Task
        :: Subsystem
        -> Priority
        -> Chunking
        -> Tier
        -> Operation status result
        -> Pattern fs m result
        -> Task



data Status
  where

    Blocked
        :: TransactionalStatus

    Queued
        :: Tier
        -> TransactionalStatus

    Running
        :: Tier
        -> Priority
        -> Chunking
        -> TransactionalStatus



data Status status result
  where

    Transacting
        :: Status
        -> status
        -> Status status result

    Running
        :: status
        -> Status status result

    Done
        :: result
        -> Status status result



data Operation status result
  where

    Operation
        :: IORef (Status status result)
        -> Operation status result



data Await
  where

    Atomically
        :: Await

    Concurrently
        :: Await



data Tasking k
  where

    FreshScope
        :: (Int -> k)
        -> Tasking k

    Task
        :: Int
        -> Pattern fs m result
        -> (Operation status result -> k)
        -> Tasking k

    Await
        :: Int
        -> Await
        -> Operation status result
        -> (result -> k)
        -> Tasking k

    Atomic
        :: Int
        -> Pattern fs m result
        -> (result -> k)
        -> Tasking k

    Stop
        :: Int
        -> Operation status result
        -> k
        -> Tasking k

    Yield
        :: Int
        -> k
        -> Tasking k

    End
        :: Int
        -> Operation status result
        -> (result -> k)
        -> Tasking k

    Update
        :: Int
        -> Operation status result
        -> status
        -> k
        -> Tasking k

    Status
        :: Int
        -> Operation status result
        -> (status -> k)
        -> Altrnating k

    Finished
        :: Operation a
        -> (Bool -> k)
        -> Tasking k

    OrElse
        :: Pattern fs m a
        -> Pattern fs m a



-- | Symbol Module

data Task fs m =
    Task
        {
          fork
              :: forall a.
                 Pattern fs m a
              -> Pattern fs m (Operation a)

        , atomically
              :: forall a.
                 Pattern fs m a
              -> Pattern fs m a

        , orElse
              :: forall a.
                 Pattern fs m a
              -> Pattern fs m a
              -> Pattern fs m a

        , yield
              :: Pattern fs m ()

        , readTVar
              :: forall a.
                 TVar a
              -> Pattern fs m a

        , writeTVar
              :: forall a.
                 TVar a
              -> a
              -> Pattern fs m ()


        }



-- | Attribute

data Taskable k =
    Taskable Int k



-- | Attribute Construct

alternator
    :: Uses Taskable gs m
    => Attribute Taskable gs m

alternator =
    Taskable 0 $ \fs ->
        let
          Taskable scope k =
              view fs

          newScope =
              succ scope

        in
          newScope `seq` return $ fs .=
             Taskable newScope k



-- | Symbol/Attribute pairing witness

instance Witnessing Taskable Tasking
  where

    witness use (Taskable i k) (FreshScope ik) =
        use k (ik i)



-- | Local Scoping Construct + Substitution

alternates
    :: Is Tasking fs m
    => (    Task fs m
         -> Pattern fs m a
       )
    -> Pattern fs m a

alternates f =
    do
      scope <- self (FreshScope id)
      rewrite scope emptyQueue $ f
          Task
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



rewrite scope =
    withQueue
  where

    withQueue queue =
        go
      where

        go (Fail e) =
            Fail e

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
