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

    -- Note: Fork creates a fresh scope; thus exception handling must be applied in-line.
    Fork
        :: {-# UNPACK #-} !Int
        -> TaskInfo scope parent
        -> Tasking k

    -- Note: Atomic creates a fresh scope; thus exception handling must be applied in-line.
    Atomic
        :: {-# UNPACK #-} !Int
        -> Pattern scope parent result
        -> Tasking k

    Await
        :: {-# UNPACK #-} !Int
        -> Operation status result
        -> (result -> k)
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
                      \child ->
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

                            self (Fork scope (TaskInfo 0 operation thread))

                , atomic =
                      \block ->
                          self (Atomic scope block)

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

                      Fork currentScope childTask@(TaskInfo _ newOp _) ->
                          check currentScope $
                               do
                                   let
                                     op =
                                         newIORef (Running Nothing)

                                   rootOperation <- fmap Operation (io op)
                                   let
                                       rootTask =
                                           TaskInfo
                                               0
                                               rootOperation
                                               (k $ unsafeCoerce newOp)
                                   
                                   queue <- io empty
                                   let
                                       addChild =
                                           insert (unsafeCoerce childTask) queue

                                       addRoot =
                                           insert (unsafeCoerce rootTask) queue

                                   io addChild
                                   io addRoot
                                   withRoot queue rootOperation


                      Yield currentScope ->
                          check currentScope $
                              let
                                  continue =
                                      k (unsafeCoerce ())

                              in
                                  start continue

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

                    go
                        :: Pattern scope parent result
                        
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

                                Just (TaskInfo quantaUsed op task) ->
                                    withOperation op quantaUsed task

                    withOperation
                        :: forall status opResult.
                           Operation status opResult
                        -> Int
                        -> Pattern scope parent opResult
                        -> Pattern scope parent result
                        
                    withOperation op@(Operation operation) =
                        withQuanta
                        where

                            withQuanta
                                :: Int
                                -> Pattern scope parent opResult
                                -> Pattern scope parent result
                                
                            withQuanta !quantaUsed =
                                run
                                where
                                  
                                    run
                                        :: Pattern scope parent opResult
                                        -> Pattern scope parent result

                                    run (Fail e) =
                                        let
                                            finish =
                                                writeIORef operation $! Failed e

                                        in
                                            do
                                                io finish
                                                go

                                    run (Pure result) =
                                        let
                                            finish =
                                                writeIORef operation $! Done $ unsafeCoerce result

                                        in
                                            do
                                                io finish
                                                go

                                    run (Super task) =
                                        let
                                            quanta =
                                                quantaUsed + 1
                                                
                                            continue =
                                                withQuanta quanta

                                        in
                                            Super (fmap continue task)

                                    run (Send symbol k) =
                                        let
                                            check currentScope continue =
                                                if currentScope == rewriteScope then
                                                    continue
                                                else
                                                    step

                                            continue next =
                                                let
                                                    !quanta =
                                                        quantaUsed + 1

                                                    rest =
                                                        TaskInfo quanta op $ unsafeCoerce next

                                                    reinsert =
                                                        insert rest queue

                                                in
                                                    do
                                                        io reinsert
                                                        go

                                            step =
                                                Send symbol (continue . k)

                                        in
                                            case prj symbol of

                                                Nothing ->
                                                    step

                                                Just x ->
                                                    case x of

                                                        Fork currentScope newTask@(TaskInfo _ newOp _) ->
                                                            check currentScope $
                                                                let
                                                                    addNewTask =
                                                                        insert (unsafeCoerce newTask) queue

                                                                    continue =
                                                                        run (k $ unsafeCoerce newOp)

                                                                in
                                                                    do
                                                                        io addNewTask
                                                                        continue

                                                        Yield currentScope ->
                                                            check currentScope $
                                                                let
                                                                    !quanta =
                                                                        quantaUsed + 1

                                                                    task =
                                                                        TaskInfo
                                                                            quanta
                                                                            (Operation operation)
                                                                            (unsafeCoerce k $ unsafeCoerce ())

                                                                    reinsert =
                                                                        insert task queue

                                                                in
                                                                    do
                                                                        io reinsert
                                                                        go

                                                        Atomic currentScope block  ->
                                                            check currentScope $
                                                                runAtomic k (unsafeCoerce block)

                                                        Stop currentScope ->
                                                            check currentScope go

                                                        _ ->
                                                            step

                                    runAtomic
                                        :: forall atomicResult.
                                           (atomicResult -> Pattern scope parent opResult)
                                        -> Pattern scope parent atomicResult
                                        -> Pattern scope parent result
                                        
                                    runAtomic afterAtomic =
                                                        withCount (0 :: Int)
                                                        where

                                                            withCount !count (Pure result) =
                                                                withQuanta (quantaUsed + count) (afterAtomic result)

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

                                                                                Fork currentScope newTask@(TaskInfo _ newOp _) ->
                                                                                    check currentScope $
                                                                                        let
                                                                                            continue =
                                                                                                k (unsafeCoerce newOp)

                                                                                            insertNew =
                                                                                                insert (unsafeCoerce newTask) queue

                                                                                        in
                                                                                            do
                                                                                                io insertNew
                                                                                                withCount count continue

                                                                                Yield currentScope ->
                                                                                    check currentScope $
                                                                                        let
                                                                                            continue =
                                                                                                k (unsafeCoerce ())

                                                                                            atomic =
                                                                                                Atomic
                                                                                                    currentScope
                                                                                                    continue

                                                                                            task =
                                                                                                Send (inj atomic) afterAtomic

                                                                                            !quanta =
                                                                                                 quantaUsed + count

                                                                                            taskInfo =
                                                                                                TaskInfo quanta op (unsafeCoerce task)

                                                                                            reinsert =
                                                                                                insert taskInfo queue

                                                                                        in
                                                                                            do
                                                                                                io reinsert
                                                                                                go

                                                                                Atomic currentScope block ->
                                                                                    check currentScope $
                                                                                        let
                                                                                            continue =
                                                                                                do
                                                                                                    result <- unsafeCoerce block
                                                                                                    k result

                                                                                        in
                                                                                            withCount count continue

                                                                                _ ->
                                                                                    ignore

                                                                        _ ->
                                                                            ignore

{-# INLINE rewrite #-}
{-# INLINE tasks #-}
{-# INLINE tasker #-}
