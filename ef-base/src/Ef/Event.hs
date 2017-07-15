{-# language BangPatterns #-}
{-# language CPP #-}
{-# language FunctionalDependencies #-}
module Ef.Event where

import Ef

import Ef.State

import Data.Queue
import Data.Promise

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import Data.Maybe
import Unsafe.Coerce

-- A small library of types and functions for multithreaded evented contexts with
-- cross-context communication, callbacks, reactivity and promises.

data Event e k where
  Become :: (e -> Ef '[Event e] c ()) -> k -> Event e k
  Continue :: Event e k
  End :: Event e k

instance Functor (Event e) where
  fmap f (Become ec k) = Become ec (f k)
  fmap f Continue = Continue
  fmap f End = End

become :: Monad c => (e -> Ef '[Event e] c ()) -> Ef '[Event e] c ()
become f = Send $ Become f (Return ())

continue :: forall e c a. Monad c => Ef '[Event e] c a
continue = Send (Continue :: Event e (Ef '[Event e] c a))

end :: forall e c a. Monad c => Ef '[Event e] c a
end = Send (End :: Event e (Ef '[Event e] c a))

type Signal ms c e = Signal' (Narrative (Messages ms) c) e

data Signal' (c :: * -> *) (e :: *)
    = Signal
        {-# UNPACK #-} !(TMVar [Behavior' c e])
    deriving Eq

{-# INLINE construct_ #-}
construct_ :: STM (Signal' c e)
construct_ = Signal <$> newTMVar []

{-# INLINE construct #-}
construct :: MonadIO m => m (Signal' c e)
construct = liftIO (Signal <$> newTMVarIO [])

{-# INLINE behaviorCount #-}
behaviorCount :: (MonadIO c')
              => Signal' c e
              -> c' Int
behaviorCount (Signal bs_) = do
  bs <- liftIO $ atomically $ readTMVar bs_
  return $ length bs

{-# INLINE nullSignal #-}
nullSignal :: (MonadIO c')
           => Signal' c e
           -> c' Bool
nullSignal (Signal bs_) = do
  bs <- liftIO $ atomically $ readTMVar bs_
  return (null bs)

{-# INLINE runner #-}
runner :: forall c c'. (MonadIO c, Monad c') => c (Signal' c' (c' ()),Behavior' c' (c' ()))
runner = liftIO $ atomically $ do
  sig <- construct_
  b   <- behavior_ sig lift
  return (sig,b)

type Behavior ms c e = Behavior' (Narrative (Messages ms) c) e
data Behavior'   c e = Behavior {-# UNPACK #-} !(TMVar (e -> Ef '[Event e] c ()))
  deriving Eq

{-# INLINE behavior_ #-}
behavior_ :: Signal' c e
         -> (e -> Ef '[Event e] c ())
         -> STM (Behavior' c e)
behavior_ sig@(Signal behaviors) newBehavior = do
  b <- Behavior <$> newTMVar newBehavior
  bs <- takeTMVar behaviors
  putTMVar behaviors (bs ++ [b])
  return b

{-# INLINE behavior #-}
behavior :: MonadIO m
         => Signal' c e
         -> (e -> Ef '[Event e] c ())
         -> m (Behavior' c e)
behavior sig@(Signal behaviors) newBehavior =
  liftIO $ atomically (behavior_ sig newBehavior)

{-# INLINE mergeS #-}
mergeS :: ( MonadIO c
          , MonadIO c'
          )
       => Signal' c e
       -> Signal' c e
       -> c' ( Signal' c e
             , Behavior' c e
             , Behavior' c e
             )
mergeS sig0 sig1 = liftIO $ atomically $ do
  sig <- construct_
  bt0 <- behavior_ sig0 $ lift . signal sig
  bt1 <- behavior_ sig1 $ lift . signal sig
  return (sig,bt0,bt1)

{-# INLINE zipS #-}
zipS :: ( MonadIO c
        , MonadIO c'
        )
     => Signal' c e
     -> Signal' c e'
     -> c' ( Signal' c (Maybe e,Maybe e')
           , Behavior' c e
           , Behavior' c e'
           )
zipS = zipWithS (\x y -> return (x,y))

{-# INLINE zipWithS #-}
zipWithS :: ( MonadIO c
            , MonadIO c'
            )
         => (Maybe e -> Maybe e' -> c x)
         -> Signal' c e
         -> Signal' c e'
         -> c' ( Signal' c x
               , Behavior' c e
               , Behavior' c e'
               )
zipWithS f sig0@(Signal bs0) sig1@(Signal bs1) = liftIO $ atomically $ do
  sig <- construct_
  c0 <- newTVar Nothing
  c1 <- newTVar Nothing
  bt0 <- behavior_ sig0 $ \e0 -> do
    mc1 <- liftIO $ atomically $ readTVar c1
    lift $ do
      e' <- f (Just e0) mc1
      signal sig e'
  bt1 <- behavior_ sig1 $ \e1 -> do
    mc0 <- liftIO $ atomically $ readTVar c0
    lift $ do
      e' <- f mc0 (Just e1)
      signal sig e'
  return (sig,bt0,bt1)

{-# INLINE mapS #-}
mapS :: ( MonadIO c
        , MonadIO c'
        )
     => Signal' c e
     -> (e -> c e')
     -> c' ( Signal' c e'
           , Behavior' c e
           )
mapS sig f = liftIO $ atomically $ do
  sig' <- construct_
  bt   <- behavior_ sig $ \e -> lift $ do
            e' <- f e
            signal sig' e'
  return (sig',bt)

{-# INLINE map2S #-}
map2S :: ( MonadIO c
         , MonadIO c'
         )
      => Signal' c e0
      -> Signal' c e1
      -> (Either e0 e1 -> c e2)
      -> c' ( Signal' c e2
            , Behavior' c e0
            , Behavior' c e1
            )
map2S sig0 sig1 f = liftIO $ atomically $ do
  sig <- construct_
  bt0 <- behavior_ sig0 $ \e0 -> lift $ do
           e0' <- f $ Left e0
           signal sig e0'
  bt1 <- behavior_ sig1 $ \e1 -> lift $ do
           e1' <- f $ Right e1
           signal sig e1'
  return (sig,bt0,bt1)

{-# INLINE filterS #-}
filterS :: ( MonadIO c
           , MonadIO c'
           )
        => Signal' c e
        -> (e -> c (Maybe e'))
        -> c' ( Signal' c e'
              , Behavior' c e
              )
filterS sig f = liftIO $ atomically $ do
  sig' <- construct_
  bt   <- behavior_ sig $ \e -> lift $ do
            me' <- f e
            forM_ me' (signal sig')
  return (sig',bt)

{-# INLINE filter2S #-}
filter2S :: ( MonadIO c
            , MonadIO c'
            )
         => Signal' c e0
         -> Signal' c e1
         -> (Either e0 e1 -> c (Maybe e))
         -> c' ( Signal' c e
               , Behavior' c e0
               , Behavior' c e1
               )
filter2S sig0 sig1 f = liftIO $ atomically $ do
  sig <- construct_
  bt0 <- behavior_ sig0 $ \e -> lift $ do
           me' <- f $ Left e
           forM_ me' (signal sig)
  bt1 <- behavior_ sig1 $ \e -> lift $ do
           me' <- f $ Right e
           forM_ me' (signal sig)
  return (sig,bt0,bt1)

{-# INLINE stop #-}
stop :: (Monad c, MonadIO c') => Behavior' c a -> c' ()
stop (Behavior b_) = liftIO $ atomically $ do
  _ <- takeTMVar b_
  putTMVar b_ (const end)

{-# INLINE signal #-}
signal :: forall c e. (MonadIO c) => Signal' c e -> e -> c ()
signal (Signal bs_) e = do
  bs <- liftIO $ atomically $ takeTMVar bs_
  bs' <- signal_ bs e
  liftIO $ atomically $ putTMVar bs_ bs'

{-# INLINE signal_ #-}
signal_ :: forall c e. MonadIO c => [Behavior' c e] -> e -> c [Behavior' c e]
signal_ bs e = do
  fmap catMaybes $ forM bs $ \(Behavior b_) -> do
    b <- liftIO $ atomically $ takeTMVar b_
    mc <- start b (b e)
    liftIO $ atomically $ putTMVar b_ $ fromMaybe (const end) mc
    return $ maybe Nothing (const $ Just $ Behavior b_) mc
  where
    start :: (e -> Ef '[Event e] c ()) -> Ef '[Event e] c () -> c (Maybe (e -> Ef '[Event e] c ()))
    start f = go
      where
        go (Return _) = return (Just f)
        go (Lift sup) = sup >>= go
        go (Do m) =
          case prj m of
            ~(Just x) ->
              case x :: Event e (Ef '[Event e] c ()) of
                Become f' k -> start (unsafeCoerce f') k
                Continue    -> return (Just f)
                End         -> return Nothing

{-# INLINE trigger #-}
trigger :: (MonadIO c) => Behavior' c e -> e -> c ()
trigger b e = void $ signal_ [b] e

-- Directly, the following types looks inherently unsafe. Technically, they are.
-- Extension of this interface to protect the construction of these types prevents
-- unsafe usage.

data Ev where Ev :: e -> {-# UNPACK #-} !(Signal' c e) -> Ev

data EvQueue where EvQueue :: {-# UNPACK #-} !(IORef (Maybe (Queue Ev))) -> EvQueue

{-# INLINE newEvQueue #-}
newEvQueue :: (MonadIO c) => c EvQueue
newEvQueue = EvQueue <$> liftIO (newIORef . Just =<< newQueue)

data DriverStopped = DriverStopped deriving Show
instance Exception DriverStopped

{-# INLINE driver #-}
driver :: (MonadIO c, '[] <: ms, Delta (Modules ts) (Messages ms))
       => EvQueue -> Object ts c -> c ()
driver (EvQueue buf) o = do
  Just qs <- liftIO $ readIORef buf
  start qs o
  where
    start q = go
      where
        go o = do
          ms <- liftIO $ handle (\(_ :: SomeException) -> return Nothing) (Just <$> collect q)
          case ms of
            Nothing -> return ()
            Just (Ev e s) -> do
              (o',_) <- o ! signal (unsafeCoerce s) e
              go o'

{-# INLINE driverPrintExceptions #-}
driverPrintExceptions :: (MonadIO c, '[] <: ms, Delta (Modules ts) (Messages ms))
                      => String -> EvQueue -> Object ts c -> c ()
driverPrintExceptions e (EvQueue buf) o = do
  Just qs <- liftIO $ readIORef buf
  start qs o
  where
    start q = go
      where
        go o = do
          ms <- liftIO $ catch (Just <$> collect q) $ \(se :: SomeException) -> do
                  putStrLn (e ++ show se)
                  return Nothing
          case ms of
            Nothing -> return ()
            Just (Ev e s) -> do
              (o',_) <- o ! signal (unsafeCoerce s) e
              go o'

data As i = As { asQueue :: EvQueue, runAs_ :: forall a. i a -> IO (Promise a) }

runAs :: MonadIO c => As i -> i a -> c (Promise a)
runAs a i = liftIO (runAs_ a i)

{-# INLINE unsafeConstructAs #-}
unsafeConstructAs :: forall c i. (MonadIO c, MonadIO i) => EvQueue -> c (As i)
unsafeConstructAs buf = do
  (sig,_) <- runner
  return $ As buf $ \i -> do
    p <- promise
    buffer buf sig $ i >>= void . fulfill p
    return p

-- Use this to create safer As constructors. For evented contexts that hold
-- their internal event buffer, EvQueue, in state, this would be used as:
--
-- > asSelf <- constructAs get
{-# INLINE constructAs #-}
constructAs :: MonadIO c => c EvQueue -> c (As c)
constructAs g = unsafeConstructAs =<< g

-- Not for external use.
{-# INLINE buffer #-}
buffer :: (MonadIO c') => EvQueue -> Signal' c e -> e -> c' ()
buffer (EvQueue buf) sig e = liftIO $ do
  mqs <- readIORef buf
  forM_ mqs $ \qs -> arrive qs $ Ev e sig

{-# INLINE killBuffer #-}
killBuffer :: (MonadIO c) => EvQueue -> c ()
killBuffer (EvQueue buf) = liftIO $ writeIORef buf Nothing

data Subscription c e = Subscription (TMVar (Maybe e,EvQueue,Signal' c e))
  deriving Eq

subscription :: MonadIO c' => EvQueue -> Signal' c e -> c' (Subscription c e)
subscription evq sig = liftIO $ Subscription <$> newTMVarIO (Nothing,evq,sig)

subscription' :: MonadIO c' => EvQueue -> Signal' c e -> e -> c' (Subscription c e)
subscription' evq sig e = liftIO $ Subscription <$> newTMVarIO (Just e,evq,sig)

issue :: MonadIO c' => Subscription c e -> e -> c' ()
issue (Subscription sub_) e = do
  (_,evq,sig) <- liftIO $ atomically $ takeTMVar sub_
  buffer evq sig e
  liftIO $ atomically $ putTMVar sub_ (Just e,evq,sig)

data SomeSubscription e where
  SomeSubscription :: Subscription c e -> SomeSubscription e

instance Eq (SomeSubscription e) where
  (==) (SomeSubscription (Subscription tmv)) (SomeSubscription (Subscription tmv')) = tmv == unsafeCoerce tmv'

data Syndicate e where
  Syndicate :: TMVar (Maybe e,[SomeSubscription e]) -> Syndicate e

instance Eq (Syndicate e) where
  (==) (Syndicate synd) (Syndicate synd') = synd == unsafeCoerce synd'

syndicate :: MonadIO c => c (Syndicate e)
syndicate = liftIO $ Syndicate <$> newTMVarIO (Nothing,[])

syndicate' :: MonadIO c => e -> c (Syndicate e)
syndicate' ev = liftIO $ Syndicate <$> newTMVarIO (Just ev,[])

nullSyndicate :: MonadIO c => Syndicate e -> c Bool
nullSyndicate (Syndicate nw) = liftIO $ atomically $ do
  ss <- takeTMVar nw
  putTMVar nw ss
  return (null ss)

joinSyndicate :: MonadIO c => Syndicate e -> Subscription c' e -> c ()
joinSyndicate (Syndicate nw) s = liftIO $ atomically $ do
  (cur,subs) <- takeTMVar nw
  putTMVar nw (cur,subs ++ [SomeSubscription s])

leaveSyndicate :: MonadIO c => Syndicate e -> Subscription c' e -> c ()
leaveSyndicate (Syndicate nw) s = liftIO $ atomically $ do
  (cur,subs) <- takeTMVar nw
  let filt = filter $ \sub -> sub /= (SomeSubscription $ unsafeCoerce s)
  putTMVar nw $ (cur,filt subs)

takeSyndicate :: MonadIO c => Syndicate e -> c (Maybe e,[SomeSubscription e])
takeSyndicate (Syndicate syn_) = liftIO $ atomically (takeTMVar syn_)

putSyndicate :: MonadIO c => Syndicate e -> (Maybe e,[SomeSubscription e]) -> c ()
putSyndicate (Syndicate syn_) syn = liftIO $ atomically (putTMVar syn_ syn)

publish :: MonadIO c => Syndicate e -> e -> c ()
publish (Syndicate nw) ev = do
  (cur,subs) <- liftIO $ atomically $ takeTMVar nw
  forM_ subs $ \(SomeSubscription (Subscription sub)) -> do
    (_,evq,sig) <- liftIO $ atomically $ takeTMVar sub
    buffer evq sig ev
    liftIO $ atomically $ putTMVar sub (Just ev,evq,sig)
  liftIO $ atomically $ putTMVar nw (Just ev,subs)

subscribe :: (MonadIO c) => Syndicate e -> c EvQueue -> c (Subscription c' e)
subscribe synd cevq = do
  evq <- cevq
  sig <- construct
  sub <- subscription evq sig
  joinSyndicate synd sub
  return sub

listen :: MonadIO c' => Subscription c e -> (e -> Ef '[Event e] c ()) -> c' (Behavior' c e)
listen (Subscription sub_) b = liftIO $ atomically $ do
  (cur,evq,sig) <- takeTMVar sub_
  b_ <- behavior_ sig b
  putTMVar sub_ (cur,evq,sig)
  return b_

type Evented = State () EvQueue

-- Be sure the `State () EvQueue` is the local context's EvQueue.
--
-- > disconnect <- connect synd $ \e -> ...
connect :: (MonadIO c,'[Evented] <: ms)
        => Syndicate e
        -> (e -> Ef '[Event e] (Ef ms c) ())
        -> Ef ms c (IO ())
connect synd f = connect_ synd get f

connect_ :: (MonadIO c', Monad c, '[Evented] <: ms)
         => Syndicate e
         -> c' EvQueue
         -> (e -> Ef '[Event e] (Ef ms c) ())
         -> c' (IO ())
connect_ synd cevq f = do
  sub <- subscribe synd cevq
  bhv <- listen sub f
  return (stop bhv >> leaveSyndicate synd sub)

-- Delay the execution of the given method by at least the given number of
-- microseconds. This is non-blocking.
delay :: forall ms c a.
         (MonadIO c, '[Evented] <: ms)
      => Int
      -> Ef ms c a
      -> Ef ms c (IO (),Promise a)
delay uSeconds c = do
  p       <- promise
  buf     <- get
  (sig,b) <- runner

  if uSeconds == 0 then
    buffer buf sig (c >>= void . fulfill p)
  else
    liftIO $ void $ forkIO $ do
      threadDelay uSeconds
      buffer buf sig (c >>= void . fulfill p)

  return (stop b,p)

-- Execute the given method locally as soon as possible, but after everything
-- currently in the event queue.
--
-- Implemented as:
--
-- > schedule = delay 0
schedule :: (MonadIO c, '[Evented] <: ms) => Ef ms c a -> Ef ms c (IO (),Promise a)
schedule = delay 0

{-# INLINE asSelf #-}
asSelf :: (MonadIO c, '[Evented] <: ms) => Ef ms c (As (Ef ms c))
asSelf = constructAs get

data Callback status result c = Callback_ (MVar (Callback_ status result c))
data Callback_ status result c = Callback
  { success :: result -> c ()
  , updates :: status -> c ()
  , failure :: SomeException -> c ()
  }

modifyCallback :: (MonadIO c')
               => Callback status result c
               -> (Callback_ status result c -> c' (Callback_ status result c))
               -> c' Bool
modifyCallback (Callback_ cb_) f = do
  mcb <- liftIO $ tryTakeMVar cb_
  case mcb of
    Nothing -> return False
    Just cb -> do
      cb' <- f cb
      liftIO $ putMVar cb_ cb'
      return True

done :: MonadIO c' => Callback status result c -> c' Bool
done (Callback_ cb) = liftIO $ isEmptyMVar cb

-- | withCallback is a primitive for constructing truly asynchronous processes.
-- Alone, `withCallback` is not especially useful. When extended to
-- cross-context execution, it becomes more useful.
withCallback :: (MonadIO c, '[Evented] <: ms)
             => (Process status result -> Ef ms c r)
             -> Callback_ status result (Ef ms c)
             -> Ef ms c (Callback status result (Ef ms c),r)
withCallback f cb0 = do
  pr <- process
  cb <- attach pr cb0
  r <- f pr
  return (cb,r)

attach :: (MonadIO c, '[Evented] <: ms)
       => Process status result
       -> Callback_ status result (Ef ms c)
       -> Ef ms c (Callback status result (Ef ms c))
attach pr cb0 = do
  self <- asSelf
  cb_ <- liftIO $ newMVar cb0
  onComplete pr $ \res -> void $ do
    Callback {..} <- takeMVar cb_
    runAs self (success res)
  onNotify pr $ \upd -> void $ do
    Callback {..} <- takeMVar cb_
    runAs self (updates upd)
  onAbort pr $ \exc -> void $ do
    Callback {..} <- takeMVar cb_
    runAs self (failure exc)
  return (Callback_ cb_)

-- onSuccess :: forall ms c status result.
--              (MonadIO c, '[Evented] <: ms)
--           => Process status result
--           -> (result -> Ef ms c ())
--           -> Ef ms c (ProcessListener status result,IO ())
-- onSuccess p f = do
--   cont <- liftIO $ newIORef True
--   buf <- get
--   (sig,bhv) <- liftIO $ atomically $ do
--     sig <- construct
--     bhv <- behavior sig (lift . f)
--     return (sig,bhv)
--   pl <- onComplete p $ \a -> do
--     shouldRun <- readIORef cont
--     when shouldRun $ buffer buf sig a
--   return (pl,stop bhv >> writeIORef cont False >> cancelListener pl)

-- onFailure :: forall ms c s r.
--              (MonadIO c, '[Evented] <: ms)
--           => Process s r
--           -> (SomeException -> Ef ms c ())
--           -> Ef ms c (ProcessListener s r,IO ())
-- onFailure p f = do
--   cont <- liftIO $ newIORef True
--   buf <- get
--   (sig,bhv) <- liftIO $ atomically $ do
--     sig <- construct
--     bhv <- behavior sig (lift . f)
--     return (sig,bhv)
--   pl <- onAbort p $ \a -> do
--     shouldRun <- readIORef cont
--     when shouldRun $ buffer buf sig a
--   return (pl,stop bhv >> writeIORef cont False >> cancelListener pl)

-- onUpdate :: forall ms c status result.
--             (MonadIO c, '[Evented] <: ms)
--          => Process status result
--          -> (status -> Ef ms c ())
--          -> Ef ms c (ProcessListener status result,IO ())
-- onUpdate p f = do
--   cont <- liftIO $ newIORef True
--   buf <- get
--   (sig,bhv) <- liftIO $ atomically $ do
--     sig <- construct
--     bhv <- behavior sig (lift . f)
--     return (sig,bhv)
--   pl <- onNotify p $ \a -> do
--     shouldRun <- readIORef cont
--     when shouldRun $ buffer buf sig a
--   return (pl,stop bhv >> writeIORef cont False >> cancelListener pl)

-- onUpdate' :: forall ms c status result.
--              (MonadIO c, '[Evented] <: ms)
--           => Process status result
--           -> (status -> Ef ms c ())
--           -> Ef ms c (ProcessListener status result,IO ())
-- onUpdate' p f = do
--   cont <- liftIO $ newIORef True
--   buf <- get
--   (sig,bhv) <- liftIO $ atomically $ do
--     sig <- construct
--     bhv <- behavior sig (lift . f)
--     return (sig,bhv)
--   pl <- onNotify' p $ \a -> do
--     shouldRun <- readIORef cont
--     when shouldRun $ buffer buf sig a
--   return (pl,stop bhv >> writeIORef cont False >> cancelListener pl)

data Shutdown = Shutdown (Syndicate ())

class With a m n | a -> m where
  using_ :: a -> n (m b -> n (Promise b))
  with_ :: a -> m b -> n (Promise b)
  shutdown_ :: a -> n ()

using :: (With a m IO, MonadIO n) => a -> n (m b -> n (Promise b))
using a = fmap (fmap liftIO) $ liftIO $ using_ a

with :: (With a m IO, MonadIO n) => a -> m b -> n (Promise b)
with a m = liftIO $ with_ a m

shutdown :: (With a m IO, MonadIO n) => a -> n ()
shutdown a = liftIO $ shutdown_ a

initialize :: (With a m IO, Monad m, MonadIO n)
           => a -> n ()
initialize a = void $ with a (return ())

connectWith :: (With w c' IO, MonadIO c', MonadIO c, '[Evented] <: ms)
            => w
            -> c' (Syndicate e)
            -> (e -> Ef '[Event e] (Ef ms c) ())
            -> Ef ms c (Promise (IO ()))
connectWith w syndicateGetter f = do
  buf <- get
  with w $ do
    syn <- syndicateGetter
    sub <- subscribe syn (return buf)
    bhv <- listen sub f
    return (stop bhv >> leaveSyndicate syn sub)

syndicateWith :: (With w c' IO, MonadIO c', MonadIO c, '[Evented] <: ms)
              => w
              -> c' (Syndicate e)
              -> e
              -> Ef ms c (Promise ())
syndicateWith w syndicateGetter e = do
  with w $ do
    nw <- syndicateGetter
    publish nw e

-- | Async simplifies cross-context callback-based computations.
--
-- > async renderer renderWithProcess Callback
-- >  { onSuccess = \r -> liftIO $ putStrLn ("Rendering result: " ++ show r)
-- >  , onFailure = \e -> liftIO $ putStrLn ("Rendering failed: " ++ show e)
-- >  , onUpdates = \n -> liftIO $ putStrLn ("Rendering progress:" ++ show n ++ "%")
-- >  }
--
-- Note that execution of a callback connected to an async process might be
-- delayed due to a non-empty event queue for the callback context. This means
-- it is possible for an async computation to complete before `onUpdates`
-- callbacks have had a chance to execute. If this is a problem, simply use raw
-- `Process`es which have callbacks of type `_ -> IO ()` that are executed
-- immediately. Note that ordering of callbacks is guaranteed by the unified
-- execution context, the calling context for `async`, unless the `Process`
-- is used in multiple threads.
async :: (With a m IO, MonadIO c, '[Evented] <: ms)
      => a
      -> (Process status result -> m ())
      -> Callback_ status result (Ef ms c)
      -> Ef ms c (Callback status result (Ef ms c))
async a f cb = fst <$> withCallback (with a . f) cb

async' :: (With a m IO, MonadIO c, '[Evented] <: ms)
       => a
       -> (Process status result -> m ())
       -> Callback_ status result (Ef ms c)
       -> Ef ms c (Callback status result (Ef ms c),Promise ())
async' a f cb = withCallback (with a . f) cb

onShutdown :: ( With a (Ef ms' IO) IO
              , MonadIO c
              , '[State () Shutdown] <: ms'
              , '[Evented] <: ms
              )
           => a
           -> Ef ms c ()
           -> Ef ms c (Promise (IO ()))
onShutdown c ons =
  connectWith c (get >>= \(Shutdown sdn) -> return sdn) (const (lift ons))

-- onSelfShutdown :: ( MonadIO c
--                 , '[Evented,State () Shutdown] <: ms
--                 )
--              => Ef ms c ()
--              -> Ef ms c (IO ())
-- onSelfShutdown ons = do
--   buf <- get
--   p <- periodical
--   Just s <- subscribe p (const $ lift ons)
--   Shutdown sdn <- get
--   joinSyndicate sdn p buf
--   return (stop s >> leaveSyndicate sdn p)

shutdownSelf :: (MonadIO c,'[State () Shutdown] <: ms) => Ef ms c ()
shutdownSelf = do
  Shutdown sdn <- get
  publish sdn ()
