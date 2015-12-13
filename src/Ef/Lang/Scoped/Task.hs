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
    , inform
    , query
    , tasks
    , tierToSteps
    , stepsToTier
    ) where



import Ef.Core
import Ef.Lang.IO
import Ef.Data.Queue

import Data.IORef
import Unsafe.Coerce



main = do
    let
      obj =
          Object $ tasker *:* Empty

    delta obj $
        tasks $ \Task {..} ->
            do
              fork Normal NonChunked $ do
                  io $ putStr "Hello"
              fork Normal NonChunked $ do
                  io $ putStr "World"
              io $ putStrLn "!"
    return ()


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
        :: Priority
        -> Chunking
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

    Fork
        :: Int
        -> Priority
        -> Chunking
        -> Operation status result
        -> Pattern scope parent result
        -> (Operation status result -> k)
        -> Tasking k

    Atomically
        :: Int
        -> Pattern scope parent result
        -> (result -> k)
        -> Tasking k

    SetPriority
        :: Int
        -> Priority
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
                 Monad parent
              => Priority
              -> Chunking
              -> Pattern scope parent result
              -> Pattern scope parent (Operation status result)

        , atomic
              :: forall result.
                 Pattern scope parent result
              -> Pattern scope parent result

        , setPriority
              :: Priority
              -> Pattern scope parent ()

        , setChunking
              :: Chunking
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
                      \priority chunking child ->
                          do
                            let
                              wrappedChild (Operation op) =
                                  do
                                    result <- child
                                    io $ writeIORef op (Done result)
                                    self (Stop scope)

                              op =
                                  newIORef (Running Nothing)

                            operation <- fmap Operation (io op)
                            let
                              thread =
                                  wrappedChild operation

                            self (Fork scope priority chunking operation thread id)

                , atomic =
                      \block ->
                          self (Atomically scope block id)

                , setPriority =
                      \priority ->
                          self (SetPriority scope priority)

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
    start
        :: Pattern scope parent result
        -> Pattern scope parent result

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

                      Fork currentScope priority chunking operation child ok ->
                          check currentScope $
                              do
                                let
                                  op =
                                      newIORef (Running Nothing)

                                rootOperation <- fmap Operation (io op)
                                let
                                    continue =
                                        ok (unsafeCoerce rootOperation)

                                    rootTask =
                                        TaskInfo
                                            Highest
                                            NonChunked
                                            rootOperation
                                            (k $ unsafeCoerce ok)

                                    childTask =
                                        TaskInfo priority chunking operation (unsafeCoerce child)

                                    queue =
                                        newQueue [rootTask,childTask]

                                withSubsystem rootOperation [(1,queue)]

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
                                              Highest
                                              newChunking
                                              rootOperation
                                              continue

                                      queue =
                                          newQueue [rootTask]

                                  withSubsystem rootOperation [(1,queue)]


                      SetPriority currentScope newPriority ->
                          check currentScope $
                              do
                                  let
                                      op =
                                          newIORef (Running Nothing)

                                  rootOperation <- fmap Operation (io op)
                                  let
                                      continue =
                                          k (unsafeCoerce ())

                                      rootTask =
                                          TaskInfo
                                              newPriority
                                              NonChunked
                                              rootOperation
                                              continue

                                      queue =
                                          newQueue [rootTask]

                                  withSubsystem rootOperation [(1,queue)]

                      _ ->
                          ignore

              _ ->
                  ignore



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
            | t > step =
                  return (tier1,slowQueue,merge (reverse acc) subsystem)

            | t == 1 =
                  do
                    (newTasks,newSlow,newTier) <- runTier 1 emptyQueue emptyQueue slowQueue queue
                    go (append tier1 newTasks) newSlow [(1,newTier)] rest

            | otherwise =
                let
                  quantaAllowed =
                      tierToSteps t

                in
                  case quantaAllowed `divMod` step of

                      (n,0) ->
                          do
                            (newTasks,newSlow,newTier) <- runTier quantaAllowed emptyQueue emptyQueue slowQueue queue
                            undefined


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
                  undefined
      where

        runTask
            :: TaskInfo scope parent
            -> Pattern scope parent (Either
                                         ( Queue (TaskInfo scope parent) )
                                         ( Queue (TaskInfo scope parent)
                                         , TaskInfo scope parent
                                         )
                                    )

        runTask ti@(TaskInfo priority chunking op@(Operation operation) task) =
            let
                quanta =
                    max (chunkingMax chunking) allowed

            in
                go emptyQueue quanta task

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
                            TaskInfo
                                priority
                                chunking
                                op
                                (unsafeCoerce task)

                        result =
                            Right (newest,continue)

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

                        ignore =
                            Send symbol (go newest (quantaLeft - 1) . k)

                    in
                        case prj symbol of

                            Nothing ->
                                ignore

                            Just x ->
                                case x of

                                    Fork currentScope priority chunking op child ok ->
                                        check currentScope $
                                            let
                                                newTask =
                                                    TaskInfo
                                                        priority
                                                        chunking
                                                        op
                                                        child

                                                newNewest =
                                                    enqueue (unsafeCoerce newTask) newest

                                                continue =
                                                    k (ok op)

                                            in
                                                go newNewest quantaLeft continue

                                    Atomically currentScope block rk ->
                                        check currentScope $
                                            do
                                                atomicResult <- try $ runAtomic priority chunking newest (unsafeCoerce block)
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

                                                    Right (newPriority,newChunking,intermediate,newNewest,quantaUsed) ->
                                                        let
                                                            newQuantaLeft =
                                                                quantaLeft - quantaUsed

                                                            newTier =
                                                                stepsToTier quantaUsed

                                                            continue =
                                                                unsafeCoerce rk intermediate

                                                        in
                                                            if newQuantaLeft > 0 then
                                                                  go newNewest newQuantaLeft continue
                                                            else
                                                                let
                                                                    newTask =
                                                                        TaskInfo
                                                                            newPriority
                                                                            newChunking
                                                                            op
                                                                            continue

                                                                    result =
                                                                        Right (newNewest,newTask)

                                                                in
                                                                    return result

                                    SetPriority currentScope newPriority ->
                                        let
                                            continue =
                                                k (unsafeCoerce ())

                                            newTask =
                                                TaskInfo newPriority chunking op continue

                                        in
                                            runTask newTask

                                    SetChunking currentScope newChunking ->
                                        let
                                            continue =
                                                k (unsafeCoerce ())

                                            newTask =
                                                TaskInfo priority newChunking op continue

                                        in
                                            runTask newTask

                                    Stop currentScope ->
                                        check currentScope $
                                            let
                                                result =
                                                    Left newest

                                            in
                                                return result

                                    Yield currentScope ->
                                        check currentScope $
                                            let
                                                result =
                                                    Right (newest,ti)

                                            in
                                                return result


    runAtomic
        :: Priority
        -> Chunking
        -> Queue (TaskInfo scope parent)
        -> Pattern scope parent result
        -> Pattern scope parent ( Priority
                                , Chunking
                                , result
                                , Queue (TaskInfo scope parent)
                                , Int
                                )

    runAtomic =
        startAtomic 0
        where
            startAtomic quanta priority chunking newest =
                go quanta
                where
                    go quantaUsed (Fail exception) =
                        Fail exception

                    go quantaUsed (Pure result) =
                        return (priority,chunking,result,newest,quantaUsed)

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
                                    newQuantaUsed =
                                        quantaUsed + 1

                                in
                                    Send symbol (go newQuantaUsed . k)

                        in
                            case prj symbol of

                                Just x ->
                                    case x of

                                        Fork currentScope priority chunking op child ok ->
                                            check currentScope $
                                                let
                                                    newTask =
                                                        TaskInfo
                                                            priority
                                                            chunking
                                                            op
                                                            child

                                                    newNewest =
                                                        enqueue (unsafeCoerce newTask) newest

                                                    continue =
                                                        k (ok op)

                                                in
                                                    startAtomic quantaUsed priority chunking newNewest continue

                                        Atomically currentScope block rk ->
                                            check currentScope $
                                                do
                                                    atomicResult <- try $ runAtomic priority chunking newest (unsafeCoerce block)
                                                    case atomicResult of

                                                        Left (exception :: SomeException) ->
                                                            Fail exception

                                                        Right (newPriority,newChunking,intermediate,newNewest,newQuantaUsed) ->
                                                            let
                                                                newNewQuantaUsed =
                                                                    quantaUsed + newQuantaUsed

                                                                newNewest =
                                                                    append newest newNewest

                                                                continue =
                                                                    unsafeCoerce rk intermediate

                                                            in
                                                                startAtomic newNewQuantaUsed newPriority newChunking newNewest continue

                                        SetPriority currentScope newPriority ->
                                            check currentScope $
                                                let
                                                    continue =
                                                        k (unsafeCoerce ())

                                                in
                                                    startAtomic quantaUsed newPriority chunking newest continue

                                        SetChunking currentScope newChunking ->
                                            check currentScope $
                                                let
                                                    continue =
                                                        k (unsafeCoerce ())

                                                in
                                                    startAtomic quantaUsed priority newChunking newest continue

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



chunkingMax (Chunked n) =
    n

chunkingMax _ =
    1


calculateTierWithPriority
    :: Priority
    -> Int
    -> Int

calculateTierWithPriority priority n =
    case priority of

        Highest ->
            1

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



mergeUpTo
    :: Int
    -> [(Int,Queue (TaskInfo scope parent))]
    -> [(Int,Queue (TaskInfo scope parent))]
    -> ([(Int,Queue (TaskInfo scope parent))],[(Int,Queue (TaskInfo scope parent))])

mergeUpTo tier from to =
    (more,to)
  where
    (less,more) =
        span (\(t,qs) -> t <= tier) from

    to =
        merge less to



merge
    :: [(Int,Queue (TaskInfo scope parent))]
    -> [(Int,Queue (TaskInfo scope parent))]
    -> [(Int,Queue (TaskInfo scope parent))]

merge subsystem [] =
    subsystem

merge ((level,newQueue):restToBeMerged) subsystem =
    go subsystem
  where

    go ((tier,queue):rest)
        | level < tier =
              (level,newQueue):merge restToBeMerged subsystem

        | level == tier =
              (tier,append queue newQueue):merge restToBeMerged rest

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
          (tier,enqueue task queue):rest

    | level < tier =
          (tier,queue):insert (level,task) rest



stepsToTier
    :: Int
    -> Int

stepsToTier n =
    let
      base =
          logBase 2 (fromIntegral n)

    in
      succ (floor base)



tierToSteps
    :: Int
    -> Int

tierToSteps n =
    2 ^ n - 1

-- | Inlines

{-# INLINE query #-}
{-# INLINE inform #-}
{-# INLINE tasks #-}
{-# INLINE rewrite #-}
{-# INLINE tasker #-}
