{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Lang.Scoped.Task
    ( Tasking
    , Task(..)
    , tasker
    , Taskable
    , inform
    , query
    , tasks
    , calculateTier
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
        :: Priority
        -> Chunking
        -> Operation status result
        -> Pattern scope parent result
        -> TaskInfo scope parent
 


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
        -> Pattern fs m result
        -> (Operation status result -> k)
        -> Tasking k

    Atomically
        :: Int
        -> Pattern fs m result
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

        , await
              :: forall status result.
                 Operation status result
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

                , await =
                      \operation ->
                          self (Await scope operation id)
                
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

                      Fork currentScope priority chunking operation child _ ->
                          check currentScope $
                              do
                                let
                                  op =
                                      newIORef (Running Nothing)

                                rootOperation <- fmap Operation (io op)
                                let
                                  rootTask =
                                      TaskInfo
                                          Highest
                                          NonChunked
                                          rootOperation
                                          (k $ unsafeCoerce rootOperation)

                                  childTask
                                      :: TaskInfo scope parent

                                  childTask =
                                      TaskInfo priority chunking operation $ unsafeCoerce child

                                  queue =
                                      newQueue [rootTask,childTask]

                                withSubsystem (unsafeCoerce rootOperation) [(1,unsafeCoerce queue)]

                      Yield currentScope ->
                          check currentScope $
                              start (k $ unsafeCoerce ())

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

        go tier slow subsystem =
            do
              (newTasks,newSlow,newSubsystem) <- execute tier slow subsystem
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
                            mergeUpToTier tier newSlow newSubsystem

                      in
                        go (tier + 1) nextSlow $
                            if isEmpty newTasks then
                                nextSubsystem
                            else
                                merge [(1,newTasks)] nextSubsystem



    execute
        :: Int
        -> [(Int,Queue (TaskInfo scope parent))]
        -> [(Int,Queue (TaskInfo scope parent))]
        -> Pattern scope parent ( Queue (TaskInfo scope parent)
                                , [(Int,Queue (TaskInfo scope parent))]
                                , [(Int,Queue (TaskInfo scope parent))]
                                )
    -- returns the new slow, new tier1, and new subsystem
    execute tier slow subsystem =
        go emptyQueue slow [] subsystem
      where
      
        go tier1 slow acc [] =
            return (tier1,slow,[])

        go tier1 slow acc subsystem@((t,queue):rest)
            | t > tier =
                  return (tier1,slow,subsystem)

            | t == 1 =
                  do
                    (newTasks,newSlow,newTier) <- evaluate slow queue
                    go (append tier1 newTasks) newSlow [(1,newTier)] rest

            | otherwise =
                let
                  ct =
                      calculateTier t
                      
                in
                  case ct `divMod` tier of

                      (n,0) -> undefined 

    evaluate slow queue = 
        undefined 

mergeUpToTier
    :: Int
    -> [(Int,Queue (TaskInfo scope parent))]
    -> [(Int,Queue (TaskInfo scope parent))]
    -> ([(Int,Queue (TaskInfo scope parent))],[(Int,Queue (TaskInfo scope parent))])

mergeUpToTier tier from0 to0 =
    (more,to)
  where
    (less,more) = 
        span (\(t,qs) -> t <= tier) from0

    to =
        merge less to0



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
