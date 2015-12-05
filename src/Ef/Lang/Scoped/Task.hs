{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Scoped.Task
    ( Tasking
    , Task(..)
    , tasker
    , Taskable
    , inform
    , query
    , isFinished
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



data Subsystem
  where

    Subsystem
        :: [(Int,Queue TaskInfo)]
        -> Subsystem



data TaskInfo
  where

    TaskInfo
        :: Priority
        -> Chunking
        -> Operation status result
        -> Pattern scope parent result
        -> TaskInfo



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
        -> k
        -> Tasking k



inform
    :: ( Lift IO parent
       , Monad parent
       )
    => Operation status result
    -> status
    -> Pattern scope parent ()

inform (Operation statusRef) newStatus =
    do
      let
        modify status =
            case status of

                Running _ ->
                    Running (Just newStatus)

                Done result ->
                    Done result

      io $ modifyIORef statusRef modify



query
    :: ( Lift IO parent
       , Monad parent
       )
    => Operation status result
    -> Pattern scope parent (Status status result)

query (Operation statusRef) =
    io (readIORef statusRef)



isFinished
    :: ( Lift IO parent
       , Monad parent
       )
    => Operation status result
    -> Pattern scope parent Bool

isFinished (Operation statusRef) =
    do
      status <- io (readIORef statusRef)
      return $
          case status of

              Running _ ->
                  False

              Done _ ->
                  True



data Task scope parent =
    Task
        {
          fork
              :: forall status result.
                 Priority
              -> Chunking
              -> Pattern scope parent result
              -> Pattern scope parent (Operation status result)

        , atomic
              :: forall result.
                 Pattern scope parent result
              -> Pattern scope parent result

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

                            operation <- io $ newIORef (Running Nothing)
                            let
                              thread =
                                  wrappedChild $ unsafeCoerce operation

                            self (Fork scope priority chunking operation thread id)

                , yield =
                      self (Yield scope ())

                , await =
                      \operation ->
                          self (Await scope operation id)
                
                }



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

                      Fork currentScope priority chunking operation child k ->
                          check currentScope $
                              do
                                rootOperation <- io $ newIORef (Running Nothing)
                                let
                                  rootTask =
                                      TaskInfo
                                          Highest
                                          NonChunked
                                          rootOperation
                                          (k operation)

                                  childTask =
                                      TaskInfo priority chunking operation child

                                  queue =
                                      newQueue [rootTask,childTask]

                                withSubsystem rootOperation [(1,queue)]

                      Await currentScope operation k ->
                          check currentScope $
                              undefined

                      Yield currentScope k ->
                          check currentScope $
                              undefined

                      Stop currentScope ->
                          check currentScope $
                              undefined

                      _ ->
                          ignore

              _ ->
                  ignore



    withSubsystem root =
        go (1 :: Int) []
      where
      
        go _ [] [] =
            do
              status <- query root
              case status of
                  -- Don't case on Running: it should be impossible

                  Failed e -> 
                      throw e

                  Done result ->
                      return result
                      
        go tier ran [] =
            go 1 [] (reverse ran)
            
        go tier ran subsystem =
            


-- | Inlines

{-# INLINE isFinished #-}
{-# INLINE query #-}
{-# INLINE inform #-}
{-# INLINE tasks #-}
{-# INLINE rewrite #-}
{-# INLINE tasker #-}
