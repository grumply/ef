{-# LANGUAGE ImpredicativeTypes #-}
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



data TaskInfo scope parent
  where

    TaskInfo
        :: {-# UNPACK #-} !QuantaUsed
        -> {-# UNPACK #-} !Priority
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
        -> {-# UNPACK #-} !Priority
        -> {-# UNPACK #-} !(Operation status result)
        -> Pattern scope parent result
        -> Tasking k

    -- Note: Focus creates a fresh scope; thus, exception handling must be applied in-line.
    Focus
        :: {-# UNPACK #-} !Int
        -> Pattern scope parent result
        -> Tasking k

    Await
        :: {-# UNPACK #-} !Int
        -> {-# UNPACK #-} !(Operation status result)
        -> Tasking k

    Stop
        :: {-# UNPACK #-} !Int
        -> Tasking k

    Yield
        :: {-# UNPACK #-} !Int
        -> Tasking k



query
    :: ( Lift IO parent
       , Monad parent
       )
    => Operation status result
    -> Pattern scope parent (Status status result)

query (Operation op) =
    io (readIORef op)



isRunning
    :: ( Lift IO parent
       , Monad parent
       )
    => Operation status result
    -> Pattern scope parent Bool

isRunning (Operation op) =
    io $
        do
            status <- readIORef op
            return $
                case status of

                    Running _ ->
                        True

                    _ ->
                        False



isDone
    :: ( Lift IO parent
       , Monad parent
       )
    => Operation status result
    -> Pattern scope parent Bool

isDone (Operation op) =
    io $
        do
            status <- readIORef op
            return $
                case status of

                    Done _ ->
                        True

                    _ ->
                        False



isFailed
    :: ( Lift IO parent
       , Monad parent
       )
    => Operation status result
    -> Pattern scope parent Bool

isFailed (Operation op) =
    io $
        do
            status <- readIORef op
            return $
                case status of

                    Failed _ ->
                        True

                    _ ->
                        False



data Ops scope parent status result =
    Ops
        {
          supplement
              :: (    Maybe status
                   -> Maybe status
                 )
              -> Pattern scope parent ()

        , inform
              :: status
              -> Pattern scope parent ()

        }


data Task scope parent =
    Task
        {
          fork
              :: forall status result.
                 Priority
              -> (    Ops scope parent status result
                   -> Pattern scope parent result
                 )
              -> Pattern scope parent (Operation status result)

        , yield
              :: Pattern scope parent ()

        , focus
              :: forall result.
                 Pattern scope parent result
              -> Pattern scope parent result

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
                          \priority child ->
                              let
                                  newOp =
                                      newIORef (Running Nothing)

                                  ops (Operation op) =
                                      Ops
                                          {
                                            supplement =
                                                \supp ->
                                                    let
                                                        alter ~(Running old) =
                                                            Running (supp old)

                                                        update =
                                                            modifyIORef op alter

                                                    in
                                                        io update

                                          , inform =
                                                \new ->
                                                    let
                                                        update =
                                                            writeIORef op (Running $ Just new)

                                                    in
                                                        io update
                                          }
                              in
                                  do
                                      operation <- fmap Operation (io newOp)
                                      self (Fork scope priority operation (child (ops operation)))

                    , focus =
                          \block ->
                              self (Focus scope block)

                    , yield =
                          self (Yield scope)

                    , await =
                          \(Operation op) ->
                              do
                                  let
                                      check
                                          :: IORef (Status status childResult)
                                          -> Pattern scope parent (Maybe (Either SomeException childResult))

                                      check op =
                                          do
                                              status <- io (readIORef op)
                                              return $
                                                  case status of

                                                      Running _ ->
                                                          Nothing

                                                      Failed exception ->
                                                          Just $ Left exception

                                                      Done result ->
                                                          Just $ Right result

                                      acquireOrYieldAndRetry =
                                          do
                                              attempt <- self (Focus scope (check op))
                                              case attempt of

                                                  Nothing ->
                                                      do
                                                          self (Yield scope)
                                                          acquireOrYieldAndRetry

                                                  Just result ->
                                                      return result

                                  acquireOrYieldAndRetry
                    }

            root =
                TaskInfo 0 Normal (Operation op) (f task)

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

          rewrite (TaskInfo quanta priority op@(Operation operation) task) =
              withOperation operation task
              where

                  withOperation
                      :: forall status taskResult.
                         IORef (Status status taskResult)
                      -> Pattern scope parent taskResult
                      -> Pattern scope parent ()
                      
                  withOperation operation =
                      run
                      where
                      
                          run
                              :: Pattern scope parent taskResult
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
                                          reinsert
                                              :: Pattern scope parent taskResult
                                              -> IO ()

                                          reinsert next =
                                              let
                                                  !newQuanta =
                                                      quanta + 1

                                                  task
                                                      :: TaskInfo scope parent
                                                  task =
                                                      TaskInfo newQuanta priority (Operation operation :: Operation status taskResult) next

                                              in
                                                   insert task queue

                                      in
                                          do
                                              Send symbol (io . reinsert . k)
                                              go

                              in
                                  case prj symbol of

                                      Nothing ->
                                          ignore

                                      Just x ->
                                          case x of

                                              Fork !currentScope childPriority childOp child ->
                                                  check currentScope $
                                                      let
                                                          task =
                                                              TaskInfo quanta childPriority childOp $ unsafeCoerce child

                                                          insertNew =
                                                              insert task queue

                                                          updated =
                                                              TaskInfo (quanta + 1) priority op $ k $ unsafeCoerce childOp

                                                          insertUpdated =
                                                              insert updated queue

                                                      in
                                                          do
                                                              io insertNew
                                                              io insertUpdated
                                                              go

                                              Yield !currentScope ->
                                                  check currentScope $
                                                      let
                                                          !newQuanta =
                                                              quanta + 1

                                                          !task =
                                                              TaskInfo newQuanta priority op (unsafeCoerce k $ unsafeCoerce ())

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
                                                                              priority
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
                                                                              priority
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

                  focus count =
                      focusing 0
                      where

                          focusing n (Pure value) =
                              let
                                  newCount =
                                      count + calculateFromPriority (n + 1)

                              in
                                  return (Just $ Left (newCount,value))

                          focusing _ (Fail e) =
                              let
                                  failure =
                                      Failed e

                                  finish =
                                      writeIORef operation failure

                              in
                                  do
                                      io finish
                                      return Nothing

                          focusing n (Super sup) =
                              Super (fmap (focus n) sup)

                          focusing n (Send symbol k) =
                              let
                                  check !currentScope continue =
                                      if currentScope == rewriteScope then
                                          continue
                                      else
                                          ignore

                                  ignore =
                                      let
                                          !newN =
                                              n + 1

                                      in
                                          Send symbol (focus newN . k)

                              in
                                  case prj symbol of

                                      Nothing ->
                                          ignore

                                      Just x ->
                                          case x of

                                              Fork !currentScope childPriority childOp child ->
                                                  check currentScope $
                                                      let
                                                          !task =
                                                              TaskInfo count childPriority childOp $ unsafeCoerce child

                                                          insertNew =
                                                              insert task queue

                                                          !newN =
                                                              n + 1

                                                      in
                                                          do
                                                              io insertNew
                                                              focusing newN (k $ unsafeCoerce childOp)

                                              Yield !currentScope ->
                                                  check currentScope $
                                                      let
                                                          !newCount =
                                                              count + calculateFromPriority priority (n + 1)

                                                          continue =
                                                              k $ unsafeCoerce ()
                                                              
                                                      in
                                                          return $ Just $ Right (newCount,continue)

                                              Focus !currentScope block ->
                                                  check currentScope $
                                                       focus count $ unsafeCoerce block



calculateFromPriority priority x =
    case priority of
      
        HighBy n -> 
            max 1 (x `div` n)

        Highest ->
            1
            
        Higher ->
            max 1 (x `div` 3)

        High ->
            max 1 (x `div` 2)

        Normal ->
            max 1 x

        Low ->
            max 1 (x * 2)

        Lower ->
            max 1 (x * 3)

        LowBy n ->
            max 1 (n * x)



{-# INLINE tasker #-}
{-# INLINE tasks #-}
