{-# LANGUAGE Trustworthy #-}
module Ef.Fiber (fiber, fibers, ThreadS(..), Operation(..), Ops(..), Threader(..), queryOp) where

import Ef
import Data.IORef
import Unsafe.Coerce

data ThreadS s r
    = ThreadRunning (Maybe s)
    | ThreadDone r

data Operation s r =
    Operation (IORef (ThreadS s r))

data Ops ms c s r = Ops
    { inform :: s -> Code ms c ()
    , supplement :: (Maybe s -> Maybe s) -> Code ms c ()
    }

queryOp :: (MonadIO c, Functor (Messages ms)) => Operation s r -> Code ms c (ThreadS s r)
queryOp (Operation op) = liftIO (readIORef op)

data Fiber k
    = Fiber Int k
    | forall r c s ms. Fork Int (Operation s r) (Code ms c r) (Operation s r -> k)
    | Yield Int k
    | forall r c ms. Focus Int (Code ms c r) (r -> k)
    | FreshScope (Int -> k)

instance Functor Fiber where
  fmap f (Fiber i k) = Fiber i (f k)
  fmap f (Fork i o c k) = Fork i o c (fmap f k)
  fmap f (Yield i k) = Yield i (f k)
  fmap f (Focus i c k) = Focus i c (fmap f k)
  fmap f (FreshScope ik) = FreshScope (fmap f ik)

instance Delta Fiber Fiber where
  delta eval (Fiber i k) (FreshScope ik) = eval k (ik i)

fibers :: (Monad c, '[Fiber] <. ts) => Fiber (Action ts c)
fibers = Fiber 0 $ \o ->
  let Module (Fiber i k) _ = o
      !i' = succ i
  in pure $ Module (Fiber i' k) o

data Threader ms c = Threader
    { fork :: forall s r. (Ops ms c s r -> Code ms c r) -> Code ms c (Operation s r)
    , wait :: forall s r. Operation s r -> Code ms c (ThreadS s r)
    , focus :: forall focusR. Code ms c focusR -> Code ms c focusR
    , yield :: Code ms c ()
    , chunk :: forall chunkR. Int -> Code ms c chunkR -> Code ms c chunkR
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
--    thread2 thread1ThreadS _ = go
--        where
--            go = do
--                s <- query thread1ThreadS
--                case s of
--                    ThreadRunning Nothing  -> ... -- hasn't started executing yet
--                    ThreadRunning (Just n) -> ... -- executing in phase n
--                    ThreadDone r      -> ... -- finished with r
--
--    fibers $ \Threader{..} -> do
--        op1 <- fork thread1
--        op2 <- fork (thread2 op1)
--        ThreadDone r <- wait op2
--        return r
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
-- overwrite the result of the other. It is easy to remedy with a call to
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
-- the n-th context is, interestingly, not atomic in the context of the n-m call
-- to fiber for all m > 0.
--
fiber :: forall ms c r. ('[Fiber] <: ms, MonadIO c) => (Threader ms c -> Code ms c r) -> Code ms c r
fiber f = do
    scope <- Send (FreshScope Return)
    let newOp = Operation <$> newIORef (ThreadRunning Nothing)
        root =
          f Threader
            { fork = \p ->
                let ops (Operation op) =
                        Ops
                        { inform = \s ->
                            let running = ThreadRunning (Just s)
                            in liftIO (writeIORef op running)
                        , supplement = \supp ->
                            let modify (ThreadRunning x) = ThreadRunning (supp x)
                            in liftIO (modifyIORef op modify)
                        }
                    newOp = Operation <$> newIORef (ThreadRunning Nothing)
                in do op <- liftIO newOp
                      Send (Fork scope op (p (ops op)) Return)

            , wait = \(Operation op) ->
                let awaiting = do
                      s <- liftIO (readIORef op)
                      case s of
                        ThreadRunning _ -> do
                            Send (Yield scope (Return ()))
                            awaiting
                        _ -> return s
                in awaiting

            , focus = \block -> Send (Focus scope block Return)

            , yield = Send (Yield scope (Return ()))
            , chunk = \chunking block ->
                let chunked = go chunking block
                    focused = Send (Focus scope chunked Return)
                    go n (Return r) = Return r
                    go 1 (Lift sup) = do
                        Send (Yield scope (Return ()))
                        let restart = go chunking
                        Lift (fmap restart sup)
                    go n (Lift sup) =
                        let continue = go (n - 1)
                        in Lift (fmap continue sup)
                    go 1 (Do m) = do
                        Send (Yield scope (Return ()))
                        Do (fmap (go chunking) m)
                    go n (Do m) =
                        Do (fmap (go (n - 1)) m)
                in focused
            }
    rootOp <- liftIO newOp
    rewrite scope rootOp (Threads [(root, rootOp)])
  where
    rewrite
        :: forall s.
           Int
        -> Operation s r
        -> ThreadRunning ms c
        -> Code ms c r
    rewrite scope (Operation rootOp) = withFibers (Threads [])
      where
        withFibers
            :: ThreadRunning ms c
            -> ThreadRunning ms c
            -> Code ms c r
        withFibers (Threads []) (Threads []) = do
            ThreadDone r <- liftIO (readIORef rootOp :: IO (ThreadS s r))
            return r
        withFibers (Threads acc) (Threads []) =
            withFibers (Threads []) (Threads $ reverse acc)
        withFibers (Threads acc) (Threads ((fiber,op@(Operation operation)):fibers)) =
            go fiber
          where
            go (Return r) = do
                let finish = writeIORef operation (ThreadDone r)
                liftIO finish
                withFibers (Threads acc) (Threads fibers)
            go (Lift sup) = Lift (fmap go sup)
            go (Do m) = do
                let check currentScope continue = if currentScope == scope then continue else ignore
                    ignore = Do $ flip fmap m $ \nxt ->
                      let newAcc = unsafeCoerce (nxt,op) : acc
                      in withFibers (Threads newAcc) (Threads fibers)
                case prj m of
                  Nothing -> ignore
                  Just x ->
                    case x of
                      Fork currentScope childOp child k ->
                        check currentScope $
                          let newAcc = unsafeCoerce (k childOp, op) : acc
                              newFibers = unsafeCoerce (child, childOp) : fibers
                          in withFibers (Threads newAcc) (Threads newFibers)

                      Yield currentScope k ->
                        check currentScope $
                          let newAcc = unsafeCoerce (k,op) : acc
                          in withFibers (Threads newAcc) (Threads fibers)

                      Focus currentScope block k -> check currentScope $ runFocus (unsafeCoerce k) (unsafeCoerce block)

                      _ -> ignore
            runFocus focusK = withNew []
              where
                withNew new (Return atomicR) =
                    let newAcc = unsafeCoerce (focusK atomicR, op) : acc
                    in withFibers (Threads newAcc) (Threads $ new ++ fibers)
                withNew new (Lift sup) = Lift (fmap (withNew new) sup)
                withNew new (Do m) =
                    let check currentScope continue = if currentScope == scope then continue else ignore
                        ignore = Do (fmap (withNew new) m)
                    in case prj m of
                           Nothing -> ignore
                           Just x ->
                               case x of
                                   Fork currentScope childOp child k ->
                                       check currentScope $
                                       let newNew = unsafeCoerce (child, childOp) : new
                                       in withNew newNew (k childOp)
                                   Yield currentScope k ->
                                       check currentScope $
                                       let continue = Send (Focus currentScope k Return) 
                                           refocus :: forall threadR. Code ms c threadR
                                           refocus = do
                                               focusR <- continue
                                               unsafeCoerce focusK focusR
                                           newAcc = unsafeCoerce (refocus, op) : acc
                                       in withFibers (Threads newAcc) (Threads fibers)
                                   Focus currentScope block k ->
                                     check currentScope $
                                       withNew new $ do
                                         intermediate <- unsafeCoerce block
                                         k intermediate
                                   _ -> ignore

data ThreadRunning ms c where
        Threads ::
          [(Code ms c threadR,
            Operation threadThreadS threadR)]
            -> ThreadRunning ms c

{-# INLINE fiber #-}
