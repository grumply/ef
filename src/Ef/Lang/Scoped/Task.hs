{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Lang.Scoped.Task
    ( Tasking
    , Task(..)
    , tasker
    , Taskable
    , Priority(..)
    , inform
    , query
    , tasks
    ) where



import Ef.Core
import Ef.Lang.IO
import Ef.Data.Queue

import Data.IORef
import Unsafe.Coerce



{-

Semantics:

atomically evaluates a pattern without yielding in the
current subsystem.

await blocks a thread awaiting the result of another thread.

yield allows a thread to transition away from evaluation to
allow other threads to evaluate.

fork takes a priority and a chunking level and queues the
child in the given tier.

threads are placed in a tier corresponding to the quanta
consumed through chunking and atomic blocks.

priorities allow a thread to consume more evaluation quanta
than their queue tier would normally allow.

-}



data TaskInfo scope parent
  where

    TaskInfo
        :: !Priority
        -> Operation status result
        -> Pattern scope parent result
        -> TaskInfo scope parent



data Priority
  where

    Highest
        :: Priority

    HighBy
        :: Int
        -> Priority

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

    LowBy
        :: Int
        -> Priority



data Status status result
  where

    Running
        :: !(Maybe status)
        -> Status status result

    Failed
        :: !SomeException
        -> Status status result

    Done
        :: !result
        -> Status status result



data Operation status result
  where

    Operation
        :: !(IORef (Status status result))
        -> Operation status result



data Tasking k
  where

    FreshScope
        :: (Int -> k)
        -> Tasking k

    Fork
        :: Int
        -> Priority
        -> Operation status result
        -> Pattern scope parent result
        -> Tasking k

    Atomically
        :: Int
        -> Pattern scope parent result
        -> Tasking k

    SetPriority
        :: Int
        -> Priority
        -> Tasking k

    Await
        :: Int
        -> Operation status result
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
                 Monad parent
              => Priority
              -> Pattern scope parent result
              -> Pattern scope parent (Operation status result)

        , atomic
              :: forall result.
                 Pattern scope parent result
              -> Pattern scope parent result

        , setPriority
              :: Priority
              -> Pattern scope parent ()

        , yield
              :: Pattern scope parent ()

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
                      \priority child ->
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

                            self (Fork scope priority operation thread)

                , atomic =
                      \block ->
                          self (Atomically scope block)

                , setPriority =
                      \priority ->
                          self (SetPriority scope priority)

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
    start begin =
        do
            let
                op =
                    newIORef (Running Nothing)

            rootOperation <- fmap Operation (io op)
            let
                rootTask =
                    TaskInfo
                        Highest
                        rootOperation
                        begin

                queue =
                    newQueue [rootTask]

            withSubsystem rootOperation [(1,queue)]


    withSubsystem
        :: Operation status result
        -> [(Int, Queue (TaskInfo scope parent))]
        -> Pattern scope parent result

    withSubsystem root startQueues =
        go 1 [] startQueues
      where

        go _ [] [] =
            do
              status <- query root
              case status of
                  -- Don't case on Running as it should be impossible
                  -- since run queue is empty.
                  Failed e ->
                      throw e

                  Done result ->
                      return result

        go _ slow [] =
            go 1 [] slow

        go step slow subsystem =
            do
              (newTasks,newSlow,newSubsystem) <- runSubsystem step slow subsystem
              case newSubsystem of

                  [] ->
                      go 1 [] $
                          if isEmpty newTasks then
                              newSlow
                          else
                              merge [(1,newTasks)] newSlow

                  _ ->
                      let
                        (nextSlow,nextSubsystem) =
                            mergeUpTo (stepsToTier step) newSlow newSubsystem

                      in
                        nextSlow `seq` nextSubsystem `seq`
                            go (step + 1) nextSlow $
                                if isEmpty newTasks then
                                    nextSubsystem
                                else
                                    merge [(1,newTasks)] nextSubsystem


    runSubsystem
        :: Int
        -> [(Int,Queue (TaskInfo scope parent))]
        -> [(Int,Queue (TaskInfo scope parent))]
        -> Pattern scope parent ( Queue (TaskInfo scope parent)
                                , [(Int,Queue (TaskInfo scope parent))]
                                , [(Int,Queue (TaskInfo scope parent))]
                                )
    -- uses step slow and subsystem
    -- returns the new slow, new tier1, and new subsystem
    runSubsystem step slow subsystem =
        go emptyQueue slow [] subsystem
      where

        go
            :: Queue (TaskInfo scope parent)
            -> [(Int,Queue (TaskInfo scope parent))]
            -> [(Int,Queue (TaskInfo scope parent))]
            -> [(Int,Queue (TaskInfo scope parent))]
            -> Pattern scope parent ( Queue (TaskInfo scope parent)
                                    , [(Int,Queue (TaskInfo scope parent))]
                                    , [(Int,Queue (TaskInfo scope parent))]
                                    )

        go tier1 slowQueue acc [] =
            return (tier1,slowQueue,reverse acc)

        go tier1 slowQueue acc subsystem@((t,queue):rest)
            | isEmpty queue =
                  go tier1 slowQueue acc rest

            | t > step =
                  return (tier1,slowQueue,merge (reverse acc) subsystem)

            | t == 1 =
                  do
                    (newTasks,newSlow,newTier) <- runTier 1 emptyQueue emptyQueue slowQueue queue
                    let
                        newTier1 =
                            append tier1 newTasks

                    newTier1 `seq`
                        go newTier1 newSlow [(1,newTier)] rest

            | otherwise =
                let
                  quantaAllowed =
                      tierToSteps t

                in
                  case quantaAllowed `divMod` step of

                      (n,0) ->
                          do
                            (newTasks,newSlow,newTier) <- runTier quantaAllowed emptyQueue emptyQueue slowQueue queue
                            let
                                !newTier1 =
                                    append tier1 newTasks

                                !mergedSlow =
                                    merge newSlow slowQueue

                                !mergedAcc =
                                    merge [(t,newTier)] acc

                            go newTier1 mergedSlow mergedAcc rest

                      _ ->
                          go tier1 slowQueue (merge [(t,queue)] acc) rest


    runTier
        :: Int
        -> Queue (TaskInfo scope parent)
        -> Queue (TaskInfo scope parent)
        -> [(Int,Queue (TaskInfo scope parent))]
        -> Queue (TaskInfo scope parent)
        -> Pattern scope parent ( Queue (TaskInfo scope parent)
                                , [(Int,Queue (TaskInfo scope parent))]
                                , Queue (TaskInfo scope parent)
                                )
    runTier allowed newTasks ran slow queue =
        case dequeue queue of

            Nothing ->
                return (newTasks,slow,ran)

            Just (task,tasks) ->
                do
                  result <- runTask task
                  case result of

                      Left newNewTasks ->
                          let
                              !news =
                                  append newTasks newNewTasks
                          in
                              runTier allowed news ran slow tasks

                      Right (quantaLeft,newNewTasks,newTask) ->
                          if quantaLeft > 0 then
                              let
                                  !appendedNews =
                                      append newTasks newNewTasks

                                  !newRan =
                                      enqueue newTask ran

                              in
                                  runTier allowed appendedNews newRan slow tasks
                          else
                              let
                                  !stepsUsed =
                                      allowed + abs quantaLeft

                                  !tier =
                                      stepsToTier stepsUsed

                                  priority =
                                      taskInfoPriority task

                                  !newTier =
                                      calculateTierWithPriority priority tier

                                  !appendedNews =
                                      append newTasks newNewTasks

                                  !newSlow =
                                      insert (newTier,newTask) slow

                              in
                                  runTier allowed appendedNews ran newSlow tasks
       where

         runTask
             :: TaskInfo scope parent
             -> Pattern scope parent (Either
                                          ( Queue (TaskInfo scope parent) )
                                          ( Int
                                          , Queue (TaskInfo scope parent)
                                          , TaskInfo scope parent
                                          )
                                     )

         runTask ti@(TaskInfo priority op@(Operation operation) task) =
             go emptyQueue allowed task

             where

                 -- ignore quanta and fail fast
                 go newest _ (Fail e) =
                     let
                         finish =
                             writeIORef operation (Failed e)

                         taskResult =
                             Left newest

                     in
                         do
                             io finish
                             return taskResult

                 -- ignore quanta and return fast
                 go newest _ (Pure result) =
                     let
                         finish =
                             writeIORef operation (Done $ unsafeCoerce result)

                         taskResult =
                             Left newest

                     in
                         do
                             io finish
                             return taskResult

                 go newest 0 task =
                     let
                         continue =
                             TaskInfo priority op (unsafeCoerce task)

                         result =
                             Right (allowed,newest,continue)

                     in
                         return result

                 -- don't count calls to Super
                 go newest quantaLeft (Super m) =
                     let
                         continue =
                             go newest quantaLeft

                     in
                         Super (fmap continue m)

                 go newest quantaLeft x@(Send symbol k) =
                     let
                         check currentScope run =
                             if currentScope == rewriteScope then
                                 run
                             else
                                 ignore
    
                         !newQuantaLeft =
                             quantaLeft - 1

                         ignore =
                             Send symbol (go newest newQuantaLeft . k)

                     in
                         case prj symbol of

                             Nothing ->
                                 ignore

                             Just x ->
                                 case x of

                                     Fork currentScope priority op child ->
                                         check currentScope $
                                             let
                                                 newTask =
                                                     TaskInfo priority op child

                                                 !newNewest =
                                                     enqueue (unsafeCoerce newTask) newest

                                                 continue =
                                                     k $ unsafeCoerce op

                                             in
                                                 go newNewest newQuantaLeft continue

                                     Atomically currentScope block ->
                                         check currentScope $
                                             do
                                                 atomicResult <- try $ runAtomic priority newest (unsafeCoerce block)
                                                 case atomicResult of

                                                     Left (e :: SomeException) ->
                                                         let
                                                             fail =
                                                                 writeIORef operation (Failed e)

                                                             result =
                                                                 Left newest

                                                         in
                                                             do
                                                                 io fail
                                                                 return result

                                                     Right (newPriority,intermediate,newNewest,quantaUsed) ->
                                                         let
                                                             !newQuantaLeft =
                                                                 quantaLeft - quantaUsed

                                                             newTier =
                                                                 stepsToTier quantaUsed

                                                             continue =
                                                                 k $ unsafeCoerce intermediate

                                                         in
                                                             if newQuantaLeft > 0 then
                                                                   go newNewest newQuantaLeft continue
                                                             else
                                                                 let
                                                                     newTask =
                                                                         TaskInfo newPriority op continue

                                                                     result =
                                                                         Right (newQuantaLeft,newNewest,newTask)

                                                                 in
                                                                     return result

                                     SetPriority currentScope newPriority ->
                                         let
                                             continue =
                                                 k (unsafeCoerce ())

                                             newTask =
                                                 TaskInfo newPriority op continue

                                         in
                                             runTask newTask

                                     Stop currentScope ->
                                         check currentScope $
                                             return (Left newest)

                                     Yield currentScope ->
                                         check currentScope $
                                             let
                                                 continue =
                                                     k (unsafeCoerce ())

                                                 newTask =
                                                     TaskInfo priority op continue

                                                 result =
                                                     Right (quantaLeft - 1,newest,newTask)

                                             in
                                                 return result


    runAtomic
        :: Priority
        -> Queue (TaskInfo scope parent)
        -> Pattern scope parent result
        -> Pattern scope parent ( Priority
                                , result
                                , Queue (TaskInfo scope parent)
                                , Int
                                )

    runAtomic =
        startAtomic 0
        where
            startAtomic quanta priority newest =
                go quanta
                where
                    go quantaUsed (Fail exception) =
                        Fail exception

                    go quantaUsed (Pure result) =
                        return (priority,result,newest,quantaUsed)

                    go quantaUsed (Super m) =
                        let
                            continue =
                                go quantaUsed

                        in
                            Super (fmap continue m)

                    go quantaUsed (Send symbol k) =
                        let
                            check currentScope run =
                                if currentScope == rewriteScope then
                                    run
                                else
                                    ignore

                            ignore =
                                let
                                    !newQuantaUsed =
                                        quantaUsed + 1

                                in
                                    Send symbol (go newQuantaUsed . k)

                        in
                            case prj symbol of

                                Just x ->
                                    case x of

                                        Fork currentScope priority op child ->
                                            check currentScope $
                                                let
                                                    newTask =
                                                        TaskInfo priority op child

                                                    newNewest =
                                                        enqueue (unsafeCoerce newTask) newest

                                                    continue =
                                                        k $ unsafeCoerce op

                                                in
                                                    newNewest `seq`
                                                         startAtomic (quantaUsed + 1) priority newNewest continue

                                        Atomically currentScope block ->
                                            check currentScope $
                                                go quantaUsed (unsafeCoerce block >>= \result -> k $ unsafeCoerce result)

                                        SetPriority currentScope newPriority ->
                                            check currentScope $
                                                let
                                                    continue =
                                                        k (unsafeCoerce ())

                                                in
                                                    
                                                    startAtomic quantaUsed newPriority newest continue

                                        Yield currentScope ->
                                            check currentScope $
                                                let
                                                    continue =
                                                        k (unsafeCoerce ())

                                                in
                                                    go quantaUsed continue

                                        _ ->
                                            ignore

                                _ ->
                                    ignore



taskInfoPriority
    :: TaskInfo scope parent
    -> Priority

taskInfoPriority (TaskInfo p _ _) =
    p
    


calculateTierWithPriority
    :: Priority
    -> Int
    -> Int

calculateTierWithPriority priority n =
    case priority of

        Highest ->
            1
            
        HighBy x ->
            min 1 (n - x)

        Higher ->
            min 1 (n - 2)

        High ->
            min 1 (n - 1)

        Normal ->
            n

        Low ->
            n + 1

        Lower ->
            n + 2
            
        LowBy x ->
            n + x



mergeUpTo
    :: Int
    -> [(Int,Queue (TaskInfo scope parent))]
    -> [(Int,Queue (TaskInfo scope parent))]
    -> ([(Int,Queue (TaskInfo scope parent))],[(Int,Queue (TaskInfo scope parent))])

mergeUpTo tier from to =
    (more,upto)
  where
    (less,more) =
        span (\(t,qs) -> t <= tier) from

    upto =
        merge less to



merge
    :: [(Int,Queue (TaskInfo scope parent))]
    -> [(Int,Queue (TaskInfo scope parent))]
    -> [(Int,Queue (TaskInfo scope parent))]

merge subsystem [] =
    subsystem

merge [] subsystem =
    subsystem

merge ((level,newQueue):restToBeMerged) subsystem =
    go subsystem
  where

    go ((tier,queue):rest)
        | level < tier =
              let
                  newSubsystem =
                      merge restToBeMerged subsystem
                      
              in
                  newSubsystem `seq`
                      (level,newQueue):newSubsystem

        | level == tier =
              let
                  newNewQueue =
                      append queue newQueue
                      
                  newRest =
                      merge restToBeMerged rest
                      
              in
                  newNewQueue `seq` newRest `seq`
                      (tier,newNewQueue):newRest

        | otherwise =
              (tier,queue):go rest



insert
    :: (Int,TaskInfo scope parent)
    -> [(Int,Queue (TaskInfo scope parent))]
    -> [(Int,Queue (TaskInfo scope parent))]

insert (level,task) [] =
    [(level,newQueue [task])]

insert (level,task) ((tier,queue):rest)
    | tier == level =
          let
              newQueue =
                  enqueue task queue
                 
          in
              newQueue `seq`
                  (tier,newQueue):rest

    | level < tier =
          let
              newRest =
                  insert (level,task) rest
          
          in
              newRest `seq`
                  (tier,queue):newRest


-- | stepsToTier converts steps to a tier.
stepsToTier
    :: Int
    -> Int

stepsToTier steps
    | steps <= 0 =
          1

    | otherwise =
          1 + floor (logBase 2 $ fromIntegral steps)



-- | tierToSteps converts a tier to a maximum steps value.
tierToSteps
    :: Int
    -> Int

tierToSteps tier
    | tier <= 1 =
          1

    | otherwise =
           pred . (2^) $ tier


-- | Inlines

{-# INLINE query #-}
{-# INLINE inform #-}
{-# INLINE tasks #-}
{-# INLINE rewrite #-}
{-# INLINE tasker #-}
{-# INLINE insert #-}
{-# INLINE merge #-}
{-# INLINE mergeUpTo #-}
{-# INLINE tierToSteps #-}
{-# INLINE stepsToTier #-}
{-# INLINE taskInfoPriority #-}
{-# INLINE calculateTierWithPriority #-}
