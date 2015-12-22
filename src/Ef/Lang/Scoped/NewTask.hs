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


import Data.IORef

import Unsafe.Coerce

import System.IO.Unsafe



type QuantaUsed =
    Int



data TaskInfo scope parent
  where

    TaskInfo
        :: {-# UNPACK #-} !QuantaUsed
        -> {-# UNPACK #-} !Chunking
        -> {-# UNPACK #-} !(Operation status result)
        -> {-# UNPACK #-} !(Pattern scope parent result)
        -> TaskInfo scope parent



instance Eq (TaskInfo scope parent)
    where

        (==) (TaskInfo qu _ _ _) (TaskInfo qu' _ _ _) =
            qu == qu'



instance Ord (TaskInfo scope parent)
    where

        compare (TaskInfo qu _ _ _) (TaskInfo qu' _ _ _) =
            compare qu qu'



data Chunking
  where

    NonChunked
        :: Chunking

    Chunked
        :: Int
        -> Chunking



data Status status result
  where

    Running
        :: Maybe status
        -> Status status result

    Failed
        :: SomeException
        -> Status status result

    Done
        :: result
        -> Status status result



data Operation status result
  where

    Operation
        :: IORef (Status status result)
        -> Operation status result



data Tasking k
  where

    FreshScope
        :: (Int -> k)
        -> Tasking k

    -- Note: Fork creates a fresh scope; thus exception handling must be applied in-line.
    Fork
        :: Int
        -> Chunking
        -> Operation status result
        -> Pattern scope parent result
        -> (Operation status result -> k)
        -> Tasking k

    -- Note: Atomic creates a fresh scope; thus exception handling must be applied in-line.
    Atomic
        :: Int
        -> Pattern scope parent result
        -> (result -> k)
        -> Tasking k

    SetChunking
        :: Int
        -> Chunking
        -> Tasking k

    Await
        :: Int
        -> Operation status result
        -> (result -> k)
        -> Tasking k

    Stop
        :: Int
        -> Tasking k

    Yield
        :: Int
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
                 Chunking
              -> Pattern scope parent result
              -> Pattern scope parent (Operation status result)

        , yield
              :: Pattern scope parent ()

        , setChunking
              :: Chunking
              -> Pattern scope parent ()

        , atomic
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



tasks
    :: ( Is Tasking scope parent
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
      rewrite scope $ f
          Task
                {
                  fork =
                      \chunking child ->
                          do
                            let
                              wrappedChild (Operation op) =
                                  do
                                    result <- child
                                    result `seq` io $ writeIORef op (Done result)
                                    self (Stop scope)

                              op =
                                  newIORef (Running Nothing)

                            operation <- fmap Operation (io op)
                            let
                              thread =
                                  wrappedChild operation

                            self (Fork scope chunking operation thread id)

                , atomic =
                      \block ->
                          self (Atomic scope block id)

                , setChunking =
                      \chunking ->
                          self (SetChunking scope chunking)

                , yield =
                      self (Yield scope)

                }


rewrite
    :: forall scope parent result.
       ( Monad parent
       , Lift IO parent
       , Is Tasking scope parent
       )
    => Int
    -> Pattern scope parent result
    -> Pattern scope parent result

rewrite rewriteScope =
    start
  where

    start (Fail e) =
        Fail e

    start (Super m) =
        Super (fmap start m)

    start (Pure result) =
        Pure result

    start (Send symbol k) =
        let
          check currentScope continue =
              if currentScope == rewriteScope then
                  continue
              else
                  ignore

          ignore =
              Send symbol (start . k)

        in
          case prj symbol of

              Just x ->
                  case x of

                      Fork currentScope chunking operation child ok ->
                          check currentScope $
                               do
                                   let
                                     op =
                                         newIORef (Running Nothing)

                                   rootOperation <- fmap Operation (io op)
                                   let
                                       result =
                                           ok (unsafeCoerce operation)

                                       rootTask =
                                           TaskInfo
                                               0
                                               NonChunked
                                               rootOperation
                                               (k result)

                                       childTask =
                                           TaskInfo
                                               0
                                               chunking
                                               operation
                                               (unsafeCoerce child)

                                   queue <- io empty
                                   io (insert childTask queue)
                                   io (insert rootTask queue)
                                   withRoot queue rootOperation


                      Yield currentScope ->
                          check currentScope $
                              let
                                  continue =
                                      k (unsafeCoerce ())

                              in
                                  start continue

                      SetChunking currentScope newChunking ->
                          check currentScope $
                              do
                                  let
                                      op =
                                          newIORef (Running Nothing)

                                  rootOperation <- fmap Operation (io op)
                                  let
                                      continue =
                                          k (unsafeCoerce rootOperation)

                                      rootTask =
                                          TaskInfo
                                              0
                                              newChunking
                                              rootOperation
                                              continue

                                      newQueue =
                                          fromList [rootTask]

                                  queue <- io newQueue
                                  withRoot queue rootOperation

                      _ ->
                          ignore

              _ ->
                  ignore
        where

            withRoot
                :: forall status.
                   Heap (TaskInfo scope parent)
                -> Operation status result
                -> Pattern scope parent result

            withRoot queue rootOperation =
                go
                where

                    go =
                        do
                            minTask <- io (extractMin queue)
                            case minTask of

                                Nothing ->
                                    do
                                        status <- query rootOperation
                                        case status of

                                            Failed e ->
                                                throw e

                                            Done result ->
                                                return result

                                Just (TaskInfo quantaUsed chunking op task) ->
                                    withOperation quantaUsed (unsafeCoerce op) chunking task



                    withOperation
                        :: forall taskResult.
                           QuantaUsed
                        -> Operation status taskResult
                        -> Chunking
                        -> Pattern scope parent taskResult
                        -> Pattern scope parent result

                    withOperation quantaUsed (Operation operation) chunking task0 =
                        case chunking of

                            NonChunked ->
                                runSteps 1 (quantaUsed + 1) task0

                            Chunked steps ->
                                runSteps steps (quantaUsed + steps) task0

                        where

                            runSteps
                                :: Int
                                -> QuantaUsed
                                -> Pattern scope parent taskResult
                                -> Pattern scope parent result

                            runSteps !_ _ (Fail e) =
                                let
                                    finish =
                                        writeIORef operation (Failed e)

                                in
                                    do
                                        io finish
                                        go

                            runSteps _ _ (Pure result) =
                                let
                                    finish =
                                        writeIORef operation (Done $ unsafeCoerce result)

                                in
                                    do
                                        io finish
                                        go

                            runSteps steps newQuantaUsed (Super task) =
                                let
                                    continue =
                                        runSteps steps newQuantaUsed

                                in
                                    Super (fmap continue task)

                            runSteps steps@((<= 0) -> True) newQuantaUsed task =
                                 let
                                     newTask =
                                         TaskInfo
                                             (newQuantaUsed - steps)
                                             chunking
                                             (Operation operation)
                                             (task)

                                     reinsert =
                                        insert (unsafeCoerce task) queue

                                 in
                                     do
                                         io reinsert
                                         go

                            runSteps steps newQuantaUsed (Send symbol k) =
                                let
                                    check currentScope continue =
                                        if currentScope == rewriteScope then
                                            continue
                                        else
                                            ignore

                                    ignore =
                                        let
                                            continue =
                                                runSteps (steps - 1) newQuantaUsed

                                        in
                                            Send symbol (continue . k)
                                in
                                    case prj symbol of

                                        Nothing ->
                                            ignore

                                        Just x ->
                                            case x of

                                                Fork currentScope newChunking newOperation child ok ->
                                                    check currentScope $
                                                        let
                                                            !newTask =
                                                                TaskInfo
                                                                    0
                                                                    newChunking
                                                                    newOperation
                                                                    child

                                                            addNewTask =
                                                                insert (unsafeCoerce newTask) queue

                                                            continue =
                                                                runSteps
                                                                    steps
                                                                    newQuantaUsed
                                                                    (k $ ok $ unsafeCoerce newOperation)
                                                        in
                                                            do
                                                                io addNewTask
                                                                continue

                                                Yield currentScope ->
                                                    check currentScope $
                                                        let
                                                            update =
                                                                insert $!
                                                                    TaskInfo
                                                                        (newQuantaUsed - steps + 1)
                                                                        chunking
                                                                        (Operation operation)
                                                                        (k $ unsafeCoerce ())

                                                        in
                                                            do
                                                                io (update queue)
                                                                go

                                                Atomic currentScope block rk ->
                                                    check currentScope $
                                                        runAtomic (unsafeCoerce rk) (unsafeCoerce block)
                                                            

                                                SetChunking currentScope chunking ->
                                                    check currentScope $
                                                        withOperation (quantaUsed + steps) (Operation operation) chunking (k $ unsafeCoerce ())
 
                                                _ ->
                                                    ignore 
                                where

                                    runAtomic afterAtomic =
                                        withCount 0
                                        where

                                            withCount !count (Pure result) =
                                                runSteps (steps - count) newQuantaUsed (afterAtomic result)

                                            withCount count (Fail e) =
                                                let
                                                    fail =
                                                        writeIORef operation (Failed e)

                                                in
                                                    do
                                                        io fail
                                                        go

                                            withCount count (Super m) =
                                                let
                                                    continue =
                                                        withCount count
  
                                                in
                                                    Super (fmap continue m)

                                            withCount count (Send symbol k) =
                                                let
                                                    check currentScope continue =
                                                        if currentScope == rewriteScope then
                                                            continue
                                                        else
                                                            ignore

                                                    ignore =
                                                        let
                                                            continue =
                                                                withCount (pred count)
  
                                                        in
                                                            Send symbol (continue . k)

                                                in
                                                    case prj symbol of

                                                        Just x ->
                                                            case x of
  
                                                                Fork currentScope newChunking newOperation child ok ->
                                                                    check currentScope $
                                                                        do
                                                                            let
                                                                                continue =
                                                                                    ok (unsafeCoerce newOperation)
   
                                                                                task =
                                                                                    TaskInfo
                                                                                        0
                                                                                        newChunking
                                                                                        newOperation
                                                                                        (unsafeCoerce child)
  
                                                                                updateQueue =
                                                                                    insert task queue

                                                                            io updateQueue
                                                                            withCount count $ k continue

                                                                Yield currentScope ->
                                                                    check currentScope $
                                                                        let
                                                                            continue =
                                                                                k (unsafeCoerce ())

                                                                            atomic =
                                                                                Atomic
                                                                                    currentScope
                                                                                    continue
                                                                                    id

                                                                            task =
                                                                                Send (inj atomic) afterAtomic

                                                                            taskInfo =
                                                                                TaskInfo
                                                                                    (newQuantaUsed - steps)
                                                                                    chunking
                                                                                    (Operation operation)
                                                                                    (unsafeCoerce task)

                                                                            reinsert =
                                                                                insert taskInfo queue

                                                                        in
                                                                            do
                                                                                io reinsert
                                                                                go

                                                                Atomic currentScope block rk ->
                                                                    check currentScope $
                                                                        withCount count (unsafeCoerce block >>= \result -> k $ rk result)

                                                                SetChunking currentScope chunking ->
                                                                    withOperation (quantaUsed + steps) (Operation operation) chunking (k $ unsafeCoerce ())

                                                                _ ->
                                                                    ignore
  
                                                        _ ->
                                                            ignore

{-# INLINE rewrite #-}
{-# INLINE tasks #-}
{-# INLINE tasker #-}
