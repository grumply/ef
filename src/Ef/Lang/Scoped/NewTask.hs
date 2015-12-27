{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Ef.Lang.Scoped.NewTask where

import Ef.Data.MinHeap
import Ef.Core
import Ef.Lang.IO

import Control.Monad
import Data.IORef

import Unsafe.Coerce

import System.IO.Unsafe

import Data.Typeable
import Debug.Trace


type QuantaUsed =
    Int



data TaskInfo scope parent
  where

    TaskInfo
        :: {-# UNPACK #-} !QuantaUsed
        -> {-# UNPACK #-} !(Operation status result)
        -> {-# UNPACK #-} !(Pattern scope parent result)
        -> TaskInfo scope parent



instance Eq (TaskInfo scope parent)
    where

        (==) (TaskInfo qu _ _) (TaskInfo qu' _ _) =
            qu == qu'



instance Ord (TaskInfo scope parent)
    where

        compare (TaskInfo qu _ _) (TaskInfo qu' _ _) =
            compare qu qu'



data Status status result
  where

    Running
        :: {-# UNPACK #-} !(Maybe status)
        -> Status status result

    Failed
        :: {-# UNPACK #-} !SomeException
        -> Status status result

    Done
        :: {-# UNPACK #-} !result
        -> Status status result



data Operation status result
  where

    Operation
        :: {-# UNPACK #-} !(IORef (Status status result))
        -> Operation status result



data Tasking k
  where

    FreshScope
        :: (Int -> k)
        -> Tasking k

    -- Note: Fork creates a fresh scope; thus, exception handling must be applied in-line.
    Fork
        :: {-# UNPACK #-} !Int
        -> Operation status result
        -> Pattern scope parent result
        -> Tasking k

    -- Note: Focus creates a fresh scope; thus, exception handling must be applied in-line.
    Focus
        :: {-# UNPACK #-} !Int
        -> Pattern scope parent result
        -> Tasking k

    Await
        :: {-# UNPACK #-} !Int
        -> Operation status result
        -> Tasking k

    Stop
        :: {-# UNPACK #-} !Int
        -> Tasking k

    Yield
        :: {-# UNPACK #-} !Int
        -> Tasking k



supplement
    :: ( Lift IO parent
       , Monad parent
       )
    => (Maybe status -> Maybe status)
    -> Operation status result
    -> Pattern scope parent ()

supplement supp (Operation statusRef) =
    let
      supplementer (Running currentStatus) =
          Running (supp currentStatus)

    in
      io (modifyIORef statusRef supplementer)



inform
    :: ( Lift IO parent
       , Monad parent
       )
    => status
    -> Operation status result
    -> Pattern scope parent ()

inform newStatus =
    supplement $ const (Just newStatus)



query
    :: ( Lift IO parent
       , Monad parent
       )
    => Operation status result
    -> Pattern scope parent (Status status result)

query (Operation statusRef) =
    io (readIORef statusRef)



data Task scope parent =
    Task
        {
          fork
              :: forall status result.
                 Pattern scope parent result
              -> Pattern scope parent (Operation status result)

        , yield
              :: Pattern scope parent ()

        , focus
              :: forall result.
                 Pattern scope parent result
              -> Pattern scope parent result

        }



data Taskable k =
    Taskable Int k



tasker
    :: Uses Taskable attrs parent
    => Attribute Taskable attrs parent

tasker =
    Taskable 0 $ \fs ->
        let
          Taskable scope k =
              view fs

          newScope =
              succ scope

        in
          newScope `seq` return $ fs .=
             Taskable newScope k



instance Witnessing Taskable Tasking
  where

    witness use (Taskable i k) (FreshScope ik) =
        use k (ik i)

adjustM
    :: ( Lift IO parent
       , Monad parent
       , Ord a
       )
    => Heap a
    -> (a -> Pattern scope parent (Maybe a))
    -> Pattern scope parent ()

adjustM heap mf =
    do
        min <- io (viewMin heap)
        case min of

            Nothing ->
                return ()

            Just m ->
                do
                    new <- mf m
                    io $
                        case new of

                            Nothing ->
                                void $ extractMin heap

                            Just n ->
                                adjust (const n) heap



tasks
    :: forall scope parent result.
       ( Is Tasking scope parent
       , Lift IO parent
       , Monad parent
       )
    => (    Task scope parent
         -> Pattern scope parent result
       )
    -> Pattern scope parent result

tasks f =
    do
        scope <- self (FreshScope id)
        queue <- io (empty :: IO (Heap (TaskInfo scope parent)))
        op <- io $ newIORef (Running Nothing)
        let
            operation =
                Operation op

            task =
                Task
                    {
                      fork =
                          \child ->
                              let
                                  op =
                                      newIORef (Running Nothing)

                              in
                                  do
                                      operation <- fmap Operation (io op)
                                      self (Fork scope operation child)

                    , focus =
                          \block ->
                              self (Focus scope block)

                    , yield =
                          self (Yield scope)

                    }

            root =
                TaskInfo 0 (Operation op) (f task)

        io (insert root queue)
        withScope queue scope
        result <- io (readIORef op)
        case result of

            Failed e ->
                throw e

            Done value ->
                return value

{-# INLINE withScope #-}
withScope
    :: forall scope parent status.
       ( Lift IO parent
       , Is Tasking scope parent
       , Monad parent
       )
    => Heap (TaskInfo scope parent)
    -> Int
    -> Pattern scope parent ()

withScope queue rewriteScope =
      go
      where

          go
              :: Pattern scope parent ()

          go =
              do
                  mayTask <- io (extractMin queue)
                  case mayTask of

                      Nothing ->
                          return ()

                      Just task ->
                          rewrite task

          rewrite
              :: TaskInfo scope parent
              -> Pattern scope parent ()

          rewrite (TaskInfo quanta op@(Operation operation) task) =
              run task
              where

                  run
                      :: forall taskResult.
                         Pattern scope parent taskResult
                      -> Pattern scope parent ()

                  run (Pure value) =
                      let
                          result =
                              Done value

                          finish =
                              writeIORef operation $ unsafeCoerce result

                      in
                          do
                              io finish
                              go

                  run (Fail e) =
                      let
                          failure =
                              Failed e

                          finish =
                              writeIORef operation failure

                      in
                          do
                              io finish
                              go

                  run (Super sup) =
                      Super (fmap run sup)

                  run (Send symbol k) =
                      let
                          check !currentScope continue =
                              if currentScope == rewriteScope then
                                  continue
                              else
                                  ignore

                          ignore =
                              let
                                  reinsert next =
                                      let
                                          !newQuanta =
                                              quanta + 1

                                          task =
                                              TaskInfo newQuanta op next

                                      in
                                           insert task queue

                              in
                                  Send symbol $ \intermediate ->
                                      let
                                           next =
                                               unsafeCoerce k intermediate

                                      in
                                          do
                                              io (reinsert next)
                                              go

                      in
                          case prj symbol of

                              Nothing ->
                                  ignore

                              Just x ->
                                  case x of

                                      Fork !currentScope childOp child ->
                                          check currentScope $
                                              let
                                                  !task =
                                                      TaskInfo quanta childOp $ unsafeCoerce child

                                                  insertNew =
                                                      insert task queue

                                                  !newQuanta =
                                                      quanta + 1

                                                  !updatedTask =
                                                      TaskInfo newQuanta op (unsafeCoerce k $ unsafeCoerce childOp)

                                                  reinsert =
                                                      insert updatedTask queue
                                              in
                                                  do
                                                      io (reinsert >> insertNew)
                                                      go

                                      Yield !currentScope ->
                                          check currentScope $
                                              let
                                                  !newQuanta =
                                                      quanta + 1

                                                  !task =
                                                      TaskInfo newQuanta op (unsafeCoerce k $ unsafeCoerce ())

                                                  reinsert =
                                                      insert task queue

                                              in
                                                  do
                                                      io reinsert
                                                      go

                                      Focus !currentScope block ->
                                          check currentScope $
                                              do
                                                  mayResult <- focus quanta $ unsafeCoerce block
                                                  case mayResult of

                                                      Nothing ->
                                                          go

                                                      Just (Left (newQuanta,value)) ->
                                                          let
                                                              task =
                                                                  TaskInfo
                                                                      newQuanta
                                                                      op
                                                                      (unsafeCoerce k value)

                                                              reinsert =
                                                                  insert task queue

                                                          in
                                                              do
                                                                  io reinsert
                                                                  go

                                                      Just (Right (newQuanta,rest)) ->
                                                          let
                                                              task =
                                                                  TaskInfo
                                                                      newQuanta
                                                                      op
                                                                      (do
                                                                          atomicResult <- self (Focus currentScope rest)
                                                                          unsafeCoerce k atomicResult
                                                                      )

                                                              reinsert =
                                                                  insert task queue

                                                         in
                                                             do
                                                                 io reinsert
                                                                 go

                  focus
                      :: forall atomicResult.
                         Int
                      -> Pattern scope parent atomicResult
                      -> Pattern scope parent (Maybe (Either (Int,atomicResult) (Int,Pattern scope parent atomicResult)))

                  focus count (Pure value) =
                      return (Just $ Left (count + 1,value))

                  focus _ (Fail e) =
                      let
                          failure =
                              Failed e

                          finish =
                              writeIORef operation failure

                      in
                          do
                              io finish
                              return Nothing

                  focus count (Super sup) =
                      Super (fmap (focus count) sup)

                  focus count (Send symbol k) =
                      let
                          check !currentScope continue =
                              if currentScope == rewriteScope then
                                  continue
                              else
                                  ignore

                          ignore =
                              let
                                  !newCount =
                                      count + 1

                              in
                                  Send symbol (focus newCount . k)

                      in
                          case prj symbol of

                              Nothing ->
                                  ignore

                              Just x ->
                                  case x of

                                      Fork !currentScope childOp child ->
                                          check currentScope $
                                              let
                                                  !task =
                                                      TaskInfo quanta childOp $ unsafeCoerce child

                                                  insertNew =
                                                      insert task queue

                                                  !newCount =
                                                      count + 1

                                              in
                                                  do
                                                      io insertNew
                                                      focus newCount (k $ unsafeCoerce childOp)

                                      Yield !currentScope ->
                                          check currentScope $
                                              return (Just $ Right (count + 1,k $ unsafeCoerce ()))

                                      Focus !currentScope block ->
                                          check currentScope $
                                               focus count $ unsafeCoerce block



{-# INLINE tasks #-}
{-# INLINE tasker #-}
