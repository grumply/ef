{-# LANGUAGE Trustworthy #-}
module Ef.Fiber (fiber, fibers, ThreadStatus(..), Operation(..), Ops(..), Threader(..))  where


import Ef
import Ef.Narrative

import Control.Lens (view,set)
import Data.IORef
import Unsafe.Coerce

-- | ThreadStatus represents the state of a thread of execution.
-- The status may be updated during
data ThreadStatus status result
    = ThreadRunning (Maybe status)
    | ThreadFailed SomeException
    | ThreadDone result

data Operation status result =
    Operation (IORef (ThreadStatus status result))

data Ops self super status result = Ops
    { inform :: status -> Narrative self super ()
    , supplement :: (Maybe status -> Maybe status) -> Narrative self super ()
    }

query
    :: ( MonadIO super
       , Monad super
       )
    => Operation status result
    -> Narrative self super (ThreadStatus status result)

query (Operation op) =
    liftIO (readIORef op)

data Fiber k
    = Fiber Int k
    | forall self status super result. Fork Int (Operation status result) (Narrative self super result)
    | Yield Int
    | forall self super result. Focus Int (Narrative self super result)
    | FreshScope (Int -> k)

instance Ma Fiber Fiber where
    ma use (Fiber i k) (FreshScope ik) = use k (ik i)

fibers :: (Monad super, '[Fiber] <. traits)
       => Trait Fiber traits super
fibers =
    Fiber 0 $ \fs ->
        let Fiber i k = view trait fs
            i' = succ i
        in i' `seq` pure $ set trait (Fiber i' k) fs

data Threader self super =
    Threader
        {
          fork :: forall status result.
                 (   Ops self super status result
                  -> Narrative self super result
                 )
              -> Narrative self super (Operation status result)

        , wait
              :: forall status result.
                 Operation status result
              -> Narrative self super (ThreadStatus status result)

        , focus
              :: forall focusResult.
                 Narrative self super focusResult
              -> Narrative self super focusResult

        , yield
              :: Narrative self super ()

        , chunk
              :: forall chunkResult.
                 Int
              -> Narrative self super chunkResult
              -> Narrative self super chunkResult
        }

-- Example use of `fibers`:
--
-- @
--    thread1 Op{..} = do
--        inform 0
--        ...
--        inform 1
--        ..
--
--    thread2 thread1ThreadStatus _ = go
--        where
--            go = do
--                status <- query thread1ThreadStatus
--                case status of
--                    ThreadRunning Nothing  -> ... -- hasn't started executing yet
--                    ThreadRunning (Just n) -> ... -- executing in phase n
--                    ThreadFailed exc       -> ... -- failed with exc
--                    ThreadDone result      -> ... -- finished with result
--
--    fibers $ \Threader{..} -> do
--        op1 <- fork thread1
--        op2 <- fork (thread2 op1)
--        result <- wait op2
--        case result of
--            ThreadFailed exception -> ...
--            ThreadDone result -> return result
-- @
--
-- Note: I suggest not using this unless you absolutely know that you need it
-- and all that its use implies, including the ease with which it allows you to
-- introduce bugs based around atomicity assumptions. For instance, imagine a
-- get-modify-put method in a shared context (forked threads share root context):
--
-- @
--   getModifyPut modify = do
--       current <- get
--       let new = modify current
--       put new
-- @
--
-- And a use case like this:
--
-- @
--   fibers $ \Threader{..} -> do
--       fork $ do
--           ...
--           getModifyPut updateFunction
--           ...
--       fork $ do
--           ...
--           getModifyPut updateFunction
--           ...
-- @
--
-- if the call to put happens to be preempted in both threads, one will likely
-- overwrite the results of the other. It is easy to remedy with a call to
-- `focus`, which is similar to a call to atomic, like this:
--
-- @
--   fibers $ \Threader{..} -> do
--       fork $ do
--           ...
--           focus $ getModifyPut updateFunction
--           ...
-- @
--
-- but it is easy to believe that your method won't need the call, which leads
-- you into the trap. This issue becomes even more difficult if you nest calls
-- to fiber and fork, in which case a call to the atomic primitive `focus` in
-- the n-th context is, interestingly, not atomic in the context of the n-1 call
-- to fiber.
--
fiber :: forall self super result.
          ('[Fiber] <: self, Monad super, MonadIO super)
       => (Threader self super -> Narrative self super result)
       -> Narrative self super result
fiber f =
  do scope <- self (FreshScope id)
     let newOp = Operation <$> newIORef (ThreadRunning Nothing)
         root =
           f Threader {fork =
                        \p ->
                          let ops (Operation op) =
                                Ops {inform =
                                       \status ->
                                         let running = ThreadRunning (Just status)
                                         in liftIO (writeIORef op running)
                                    ,supplement =
                                       \supp ->
                                         let modify (ThreadRunning x) =
                                               ThreadRunning (supp x)
                                         in liftIO (modifyIORef op modify)}
                              newOp = Operation <$> newIORef (ThreadRunning Nothing)
                          in do op <- liftIO newOp
                                self (Fork scope op (p (ops op)))
                     ,wait =
                        \(Operation op) ->
                          let awaiting =
                                do status <- liftIO (readIORef op)
                                   case status of
                                     ThreadRunning _ ->
                                       do self (Yield scope)
                                          awaiting
                                     _ -> return status
                          in awaiting
                     ,focus = \block -> self (Focus scope block)
                     ,yield = self (Yield scope)
                     ,chunk =
                        \chunking block ->
                          let chunked = go chunking block
                              focused = self (Focus scope chunked)
                              go n (Return result) = Return result
                              go n (Fail exception) = Fail exception
                              go 1 (Super sup) =
                                do self (Yield scope)
                                   let restart = go chunking
                                   Super (fmap restart sup)
                              go n (Super sup) =
                                let continue = go (n - 1)
                                in Super (fmap continue sup)
                              go 1 (Say symbol k) =
                                do self (Yield scope)
                                   Say symbol (go chunking . k)
                              go n (Say symbol k) =
                                let continue value = go newN (k value)
                                    newN = n - 1
                                in Say symbol continue
                          in focused}
     rootOp <- liftIO newOp
     rewrite scope rootOp (Threads [(root,rootOp)])
  where
    rewrite :: forall status.
               Int
            -> Operation status result
            -> ThreadRunning self super
            -> Narrative self super result

    rewrite scope (Operation rootOp) = withFibers (Threads [])
      where withFibers :: ThreadRunning self super
                       -> ThreadRunning self super
                       -> Narrative self super result
            withFibers (Threads []) (Threads []) =
              do result <- liftIO (readIORef rootOp :: IO (ThreadStatus status result))
                 case result of
                   ThreadFailed exception -> throwM exception
                   ~(ThreadDone result) -> return result
            withFibers (Threads acc) (Threads []) =
              withFibers (Threads [])
                         (Threads $ reverse acc)
            withFibers (Threads acc) (Threads ((fiber,op@(Operation operation)):fibers)) =
              go fiber
              where go (Return result) =
                      let finish =
                            writeIORef operation
                                       (ThreadDone result)
                      in do liftIO finish
                            withFibers (Threads acc)
                                       (Threads fibers)
                    go (Fail exception) =
                      let fail =
                            writeIORef operation
                                       (ThreadFailed exception)
                      in do liftIO fail
                            withFibers (Threads acc)
                                       (Threads fibers)
                    go (Super sup) = Super (fmap go sup)
                    go (Say symbol k) =
                      let check currentScope continue =
                            if currentScope == scope
                               then continue
                               else ignore
                          ignore =
                            Say symbol $
                            \intermediate ->
                              let continue = k intermediate
                                  ran = (continue,op)
                                  newAcc = unsafeCoerce ran : acc
                              in withFibers (Threads newAcc)
                                            (Threads fibers)
                      in case prj symbol of
                           Nothing -> ignore
                           Just x ->
                             case x of
                               Fork currentScope childOp child ->
                                 check currentScope $
                                 let newAcc =
                                       unsafeCoerce (k $ unsafeCoerce childOp,op) :
                                       acc
                                     newFibers =
                                       unsafeCoerce (child,childOp) : fibers
                                 in withFibers (Threads newAcc)
                                               (Threads newFibers)
                               Yield currentScope ->
                                 let newAcc =
                                       unsafeCoerce (k $ unsafeCoerce (),op) : acc
                                 in withFibers (Threads newAcc)
                                               (Threads fibers)
                               Focus currentScope block ->
                                 check currentScope $
                                 runFocus (unsafeCoerce k)
                                          (unsafeCoerce block)
                               _ -> ignore
                    runFocus focusK = withNew []
                      where withNew new (Return atomicResult) =
                              let newAcc =
                                    unsafeCoerce
                                      (focusK $ unsafeCoerce atomicResult,op) :
                                    acc
                              in withFibers (Threads newAcc)
                                            (Threads $ new ++ fibers)
                            withNew new (Fail exception) =
                              let fail =
                                    writeIORef operation
                                               (ThreadFailed exception)
                              in do liftIO fail
                                    withFibers (Threads acc)
                                               (Threads $ new ++ fibers)
                            withNew new (Super sup) =
                              let continue = withNew new
                              in Super (fmap continue sup)
                            withNew new (Say symbol k) =
                              let check currentScope continue =
                                    if currentScope == scope
                                       then continue
                                       else ignore
                                  ignore =
                                    let continue intermediate =
                                          withNew new (k intermediate)
                                    in Say symbol continue
                              in case prj symbol of
                                   Nothing -> ignore
                                   Just x ->
                                     case x of
                                       Fork currentScope childOp child ->
                                         check currentScope $
                                         let newNew =
                                               unsafeCoerce (child,childOp) : new
                                         in withNew newNew
                                                    (k $ unsafeCoerce childOp)
                                       Yield currentScope ->
                                         check currentScope $
                                         let continue =
                                               self (Focus currentScope $
                                                     k $ unsafeCoerce ())
                                             refocus
                                               :: forall threadResult.
                                                  Narrative self super threadResult
                                             refocus =
                                               do focusResult <- continue
                                                  unsafeCoerce $
                                                    focusK $
                                                    unsafeCoerce focusResult
                                             newAcc =
                                               unsafeCoerce (refocus,op) : acc
                                         in withFibers (Threads newAcc)
                                                       (Threads fibers)
                                       Focus currentScope block ->
                                         check currentScope $
                                         withNew new $
                                         do intermediate <- unsafeCoerce block
                                            k $ unsafeCoerce intermediate
                                       _ -> ignore
data ThreadRunning self super
    where

        Threads
            :: [(Narrative self super threadResult, Operation threadThreadStatus threadResult)]
            -> ThreadRunning self super

{-# INLINE fiber #-}
