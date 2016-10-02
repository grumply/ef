{-# language BangPatterns #-}
module Ef.Event where

import Ef
import Ef.Narrative

import Data.Queue
import Data.Promise

import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Unsafe.Coerce

import Control.Exception (BlockedIndefinitelyOnSTM(..))

-- A highly simplified implementation of reactive programming; behaviors
-- are in an Event monad that permits behavior self-modification including
-- short-circuiting, exit, and switching as well as secondary signaling.
-- Signals are a sequence of mutable behaviors. Signals do not hold on
-- to old values to improve GC. Behaviors are the mutable references that
-- are stored in the Signal; they do not hold onto the Signal itself to
-- improve GC. When combining signals, a weak reference is created to hold
-- the latest values. The weak references is keyed on the upstream signal.
-- Subsignal/secondary signaling tries to be productive in that events are
-- run breadth first rather than depth first.

data Event k where
  Become :: (event -> Narrative '[Event] super ())
         -> k
         -> Event k

  Continue :: Event k

  End :: Event k

  Subsignal :: Signal' super event'
            -> event'
            -> k
            -> Event k

become :: Monad super
      => (event -> Narrative '[Event] super ())
      -> Narrative '[Event] super ()
become f = self $ Become f ()

continue :: Monad super => Narrative '[Event] super a
continue = self Continue

end :: Monad super => Narrative '[Event] super a
end = self End

subsignal :: Monad super
          => Signal' super event
          -> event
          -> Narrative '[Event] super ()
subsignal sig e = self $ Subsignal sig e ()

type Signal self super event = Signal' (Narrative self super) event

data Signal' super event
    = Signal
        (IORef [IORef (event -> Narrative '[Event] super ())])
    deriving Eq

{-# INLINE behaviorCount #-}
behaviorCount :: (Monad super', MonadIO super')
              => Signal' super event
              -> super' Int
behaviorCount (Signal bs_) = do
  bs <- liftIO $ readIORef bs_
  return $ length bs

{-# INLINE nullSignal #-}
nullSignal :: (Monad super', MonadIO super')
           => Signal' super event
           -> super' Bool
nullSignal (Signal bs_) = do
  bs <- liftIO $ readIORef bs_
  return (null bs)

type Behavior self super event = Behavior' (Narrative self super) event

data Behavior' super event
  = Behavior
      (IORef (event -> Narrative '[Event] super ()))
  deriving Eq

{-# INLINE construct #-}
construct :: (Monad super', MonadIO super')
          => super' (Signal' super event)
construct = liftIO $ do
  behaviors <- newIORef []
  return $ Signal behaviors

{-# INLINE constructSelf #-}
constructSelf :: (Monad super, MonadIO super)
              => Narrative self super (Signal self super event)
constructSelf = construct

-- Slightly more specific than necessary to avoid required type signatures.
{-# INLINE runner #-}
runner :: forall self super super'.
          (Monad super, MonadIO super, Monad super')
       => super (Signal' super' (super' ()))
runner = liftIO $ do
  bhvr <- newIORef lift
  behaviors <- newIORef $ [bhvr]
  return $ Signal behaviors

{-# INLINE behavior #-}
behavior :: (Monad super', MonadIO super')
         => Signal' super event
         -> (event -> Narrative '[Event] super ())
         -> super' (Behavior' super event)
behavior sig@(Signal behaviors) newBehavior = liftIO $ do
  b <- newIORef newBehavior
  modifyIORef behaviors (++ [b])
  return (Behavior b)

{-# INLINE mergeS #-}
mergeS :: ( Monad super, MonadIO super, MonadThrow super
          , Monad super', MonadIO super'
          )
       => Signal' super event
       -> Signal' super event
       -> super' ( Signal' super event
                 , Behavior' super event
                 , Behavior' super event
                 )
mergeS sig0 sig1 = do
  sig <- construct
  bt0 <- behavior sig0 $ lift . signal sig
  bt1 <- behavior sig1 $ lift . signal sig
  return (sig,bt0,bt1)

{-# INLINE zipS #-}
zipS :: ( Monad super, MonadIO super, MonadThrow super
        , Monad super', MonadIO super'
        )
     => Signal' super event
     -> Signal' super event'
     -> super' ( Signal' super (Maybe event,Maybe event')
               , Behavior' super event
               , Behavior' super event'
               )
zipS = zipWithS (\x y -> return (x,y))

{-# INLINE zipWithS #-}
zipWithS :: ( Monad super, MonadIO super, MonadThrow super
            , Monad super', MonadIO super'
            )
         => (Maybe event -> Maybe event' -> super x)
         -> Signal' super event
         -> Signal' super event'
         -> super' ( Signal' super x
                   , Behavior' super event
                   , Behavior' super event'
                   )
zipWithS f sig0@(Signal bs0) sig1@(Signal bs1) = do
  sig <- construct
  c0 <- liftIO $ newIORef Nothing
  c1 <- liftIO $ newIORef Nothing
  bt0 <- behavior sig0 $ \e0 -> do
    mc1 <- liftIO $ readIORef c1
    lift $ do
      e' <- f (Just e0) mc1
      signal sig e'
  bt1 <- behavior sig1 $ \e1 -> do
    mc0 <- liftIO $ readIORef c0
    lift $ do
      e' <- f mc0 (Just e1)
      signal sig e'
  return (sig,bt0,bt1)

{-# INLINE mapS #-}
mapS :: ( Monad super, MonadIO super, MonadThrow super
        , Monad super', MonadIO super'
        )
     => Signal' super event
     -> (event -> super event')
     -> super' ( Signal' super event'
               , Behavior' super event
               )
mapS sig f = do
  sig' <- construct
  bt   <- behavior sig $ \e -> lift $ do
            e' <- f e
            signal sig' e'
  return (sig',bt)

{-# INLINE map2S #-}
map2S :: ( Monad super, MonadIO super, MonadThrow super
         , Monad super', MonadIO super'
         )
      => Signal' super event0
      -> Signal' super event1
      -> (Either event0 event1 -> super event2)
      -> super' ( Signal' super event2
                , Behavior' super event0
                , Behavior' super event1
                )
map2S sig0 sig1 f = do
  sig <- construct
  bt0 <- behavior sig0 $ \e0 -> lift $ do
           e0' <- f $ Left e0
           signal sig e0'
  bt1 <- behavior sig1 $ \e1 -> lift $ do
           e1' <- f $ Right e1
           signal sig e1'
  return (sig,bt0,bt1)

{-# INLINE filterS #-}
filterS :: ( Monad super, MonadIO super, MonadThrow super
           , Monad super', MonadIO super'
           )
        => Signal' super event
        -> (event -> super (Maybe event'))
        -> super' ( Signal' super event'
                  , Behavior' super event
                  )
filterS sig f = do
  sig' <- construct
  bt   <- behavior sig $ \e -> lift $ do
            me' <- f e
            forM_ me' (signal sig')
  return (sig',bt)

{-# INLINE filter2S #-}
filter2S :: ( Monad super, MonadIO super, MonadThrow super
            , Monad super', MonadIO super'
            )
         => Signal' super event0
         -> Signal' super event1
         -> (Either event0 event1 -> super (Maybe event))
         -> super' ( Signal' super event
                   , Behavior' super event0
                   , Behavior' super event1
                   )
filter2S sig0 sig1 f = do
  sig <- construct
  bt0 <- behavior sig0 $ \e -> lift $ do
           me' <- f $ Left e
           forM_ me' (signal sig)
  bt1 <- behavior sig1 $ \e -> lift $ do
           me' <- f $ Right e
           forM_ me' (signal sig)
  return (sig,bt0,bt1)

{-# INLINE duplicate #-}
duplicate :: (Monad super', MonadIO super')
          => Behavior' super event
          -> Signal' super event
          -> super' ()
duplicate (Behavior b) (Signal bs_) = liftIO $ modifyIORef bs_ (++ [b])

-- stop is a delayed effect that doesn't happen until the next event, but all
-- internally maintained references are released; stopping an un-needed behavior
-- can permit GC external to the behavior itself.
{-# INLINE stop #-}
stop :: (Monad super, Monad super', MonadIO super')
     => Behavior' super a -> super' ()
stop (Behavior b_) = liftIO $ atomicModifyIORef' b_ $ const (const end,())

data Runnable super where
  Runnable :: IORef [(IORef (event -> Narrative '[Event] super ()))]
           -> IORef (event -> Narrative '[Event] super ())
           -> Narrative '[Event] super ()
           -> Runnable super

{-# INLINE signal #-}
signal :: forall super event.
          (Monad super, MonadIO super, MonadThrow super)
       => Signal' super event
       -> event
       -> super ()
signal sig e = do
  let Signal bs_ = sig
  bs <- liftIO $ readIORef bs_
  seeded <- forM bs $ \f_ -> do
    f <- liftIO $ readIORef f_
    return $ Runnable bs_ f_ (f e)
  signal_ seeded

{-# INLINE signal_ #-}
signal_ :: forall super.
           (Monad super, MonadIO super, MonadThrow super)
        => [Runnable super] -> super ()
signal_ [] = return ()
signal_ (r@(Runnable bs_ f_ f):rs) = start rs r
  where
    start :: [Runnable super] -> Runnable super -> super ()
    start rs (Runnable bs_ f_ f) = go' f
      where
        go' :: Narrative '[Event] super ()
            -> super ()
        go' (Return _) = signal_ rs

        -- One behavior can clobber an entire event;
        -- this is expected behavior since a signal is
        -- seen as a unification of possibly-multiple
        -- seemingly disjoint processes. This is an
        -- important point, however; when constructing
        -- a behavior, keep in mind that it has effects
        -- on the signal it is connected to, but /only/
        -- through 'throwM'. This may also be used to
        -- interesting effect; one may set up a behavior
        -- that guarantees some predicate, i.e. if the
        -- predicate does not succeed then the behavior
        -- throws an exception and the rest of the
        -- behaviors after that will not run; a security
        -- mechanism.
        go' (Fail e)    = throwM e

        go' (Super sup) = sup >>= go'
        go' (Say msg k) =
          case prj msg of
            ~(Just x) ->
              case x of
                Become f' x -> do
                  liftIO $ writeIORef f_ $ unsafeCoerce f'
                  go' (k x)
                Continue -> signal_ rs
                End -> do
                  liftIO $ do
                  -- in case we arrived here through trigger, nullify the behavior.
                    writeIORef f_ (const end)
                    modifyIORef bs_ $ Prelude.filter (/= f_)
                  signal_ rs
                Subsignal sig' e' x -> do
                  let Signal bs'_ = sig'
                  seeded <- liftIO $ do
                    bs' <- readIORef bs'_
                    forM bs' $ \f'_ -> do
                      f' <- readIORef f'_
                      return (Runnable bs'_ f'_ (f' e'))
                  signal_ ((Runnable bs_ f_ (k x) : rs) ++ unsafeCoerce seeded)

{-# INLINE trigger #-}
trigger :: (Monad super, MonadIO super, MonadThrow super)
        => Behavior' super event
        -> event
        -> super ()
trigger (Behavior b_) e = do
  b <- liftIO $ readIORef b_
  bs_ <- liftIO $ newIORef []
  signal_ [Runnable bs_ b_ (b e)]

data Periodical' super event
  = Periodical (IORef (Maybe (IORef (Maybe event),Signal' super event)))
  deriving Eq

type Periodical self super event = Periodical' (Narrative self super) event

type Subscription' super event = Behavior' super event

type Subscription self super event = Behavior self super event

periodical :: (Monad super, MonadIO super)
             => super (Periodical' super' event)
periodical = do
  sig <- construct
  cur_ <- liftIO $ newIORef Nothing
  p <- liftIO $ newIORef (Just (cur_,sig))
  return $ Periodical p

periodical' :: (Monad super, MonadIO super)
             => event -> super (Periodical' super' event)
periodical' ev = do
  sig <- construct
  cur_ <- liftIO $ newIORef (Just ev)
  p <- liftIO $ newIORef (Just (cur_,sig))
  return $ Periodical p

nullPeriodical :: (Monad super, MonadIO super)
               => Periodical' super' event -> super Bool
nullPeriodical (Periodical p_) = do
  mp <- liftIO $ readIORef p_
  return $ isNothing mp

emptyPeriodical :: (Monad super, MonadIO super)
                => Periodical' super event -> super Bool
emptyPeriodical (Periodical p_) = do
  mp <- liftIO $ readIORef p_
  maybe (return True) (nullSignal . snd) mp

subscribe :: (Monad super', MonadIO super')
          => Periodical' super event
          -> (event -> Narrative '[Event] super ())
          -> super' (Maybe (Subscription' super event))
subscribe (Periodical p_) f = do
  mp <- liftIO $ readIORef p_
  forM mp $ \(_,sig) -> behavior sig f

-- like subscribe, but immediately trigger the behavior with the current value
subscribe' :: (Monad super, MonadIO super, MonadThrow super)
           => Periodical' super event
           -> (event -> Narrative '[Event] super ())
           -> super (Maybe (Subscription' super event,Bool))
subscribe' (Periodical p_) f = do
  mp <- liftIO $ readIORef p_
  forM mp $ \(cur_,sig) -> do
    bt <- behavior sig f
    mcur <- liftIO $ readIORef cur_
    case mcur of
      Nothing -> return (bt,False)
      Just cur -> do
        trigger bt cur
        return (bt,True)

publish :: (Monad super, MonadIO super, MonadThrow super)
        => Periodical' super event
        -> event
        -> super Bool
publish (Periodical p_) ev = do
  mp <- liftIO $ readIORef p_
  case mp of
    Nothing -> return False
    Just (cur_,sig) -> do
      liftIO $ writeIORef cur_ (Just ev)
      signal sig ev
      return True

-- subpublish in a multithreaded environment can cause desynchronization;
-- the current value could be a value that has not yet been seen by the signal
-- what is the solution?
subpublish :: (Monad super, MonadIO super, MonadThrow super)
           => Periodical' super event -> event -> Narrative '[Event] super Bool
subpublish (Periodical p_) ev = do
  mp <- liftIO $ readIORef p_
  case mp of
    Nothing -> return False
    Just (cur_,sig) -> do
      liftIO $ writeIORef cur_ (Just ev)
      subsignal sig ev
      return True

periodicalCurrent :: (Monad super, MonadIO super)
                  => Periodical' super event -> super (Maybe event)
periodicalCurrent (Periodical p_) = liftIO $ do
  mp <- readIORef p_
  case mp of
    Just (cur_,_) -> readIORef cur_
    _ -> return Nothing

data Syndicated event where
  Syndicated :: Periodical' super event -> Signaled -> Syndicated event

-- syndicated periodical network; periodicals that share a common event;
-- event propagation network for concurrent dispatch
data Network event
  = Network (IORef (Maybe event)) (IORef [Syndicated event])
  deriving Eq

-- create a new syndication network
network :: (Monad super, MonadIO super)
        => super (Network event)
network = liftIO $ do
  mcur <- newIORef Nothing
  sd <- newIORef []
  return $ Network mcur sd

-- create a new syndication network
network' :: (Monad super, MonadIO super)
        => event -> super (Network event)
network' ev = liftIO $ do
  mcur <- newIORef (Just ev)
  sd <- newIORef []
  return $ Network mcur sd

nullNetwork :: (Monad super, MonadIO super)
            => Network event -> super Bool
nullNetwork (Network _ ss_)= liftIO $ do
  ss <- readIORef ss_
  return (null ss)

-- syndicate an event across multiple periodicals in a network
syndicate :: (Monad super, MonadIO super)
          => Network event -> event -> super ()
syndicate (Network mcur_ sd_) ev = liftIO $ do
  writeIORef mcur_ (Just ev)
  sds <- readIORef sd_
  case sds of
    [] ->
      return ()
    xs -> do
      sds' <- forM xs $ \s@(Syndicated (Periodical p_) sigd) -> do
        mp <- readIORef p_
        case mp of
          Nothing -> return Nothing
          Just (cur_,sig) -> do
            writeIORef cur_ (Just ev)
            bufferIO sigd sig ev
            return $ Just s
      writeIORef sd_ $ catMaybes sds'

networkCurrent :: (Monad super, MonadIO super) => Network event -> super (Maybe event)
networkCurrent (Network me_ _) = liftIO $ readIORef me_

joinNetwork :: (Monad super, MonadIO super)
            => Network event -> Periodical' super' event -> Signaled -> super ()
joinNetwork (Network mcur_ sd_) pl sg = do
  liftIO $ modifyIORef sd_ $ \sd -> sd ++ [Syndicated pl sg]

joinNetwork' :: (Monad super, MonadIO super)
             => Network event -> Periodical' super' event -> Signaled -> super Bool
joinNetwork' ntw@(Network mcur_ sd_) pl@(Periodical p_) sigd = liftIO $ do
  mp <- readIORef p_
  case mp of
    Nothing -> return False
    Just (cur_,sig) -> do
      modifyIORef sd_ $ \sd -> sd ++ [Syndicated pl sigd]
      mcur <- readIORef mcur_
      case mcur of
        Nothing -> return False
        Just cur -> do
          writeIORef cur_ (Just cur)
          bufferIO sigd sig cur
          return True

leaveNetwork :: (Monad super, MonadIO super)
             => Network event -> Periodical' super' event -> super ()
leaveNetwork (Network mcur_ sd_) pl =
  liftIO $
    -- let's hope this works.... if not, just create a counter token or make
    -- Network (IORef [IORef (Maybe (Syndicated event))]) or something
    modifyIORef sd_ $ \sd ->
      filter (\(Syndicated pl' _) -> pl /= unsafeCoerce pl') sd

-- An abstract Signal queue. Useful for building event loops.
-- Note that there is still a need to call unsafeCoerce on the
-- Signal itself since this data type avoids having `self` and
-- `super` as type variables. It is the responsibility of the
-- programmer to know how to use this safely; single-responsibility
-- for both injection and extraction with unified typing is required.
data Signaling where
    Signaling :: [e] -> Signal' super e -> Signaling
data Signaled where
    Signaled :: Queue Signaling -> Signaled

data As internal external
  = As { signaledAs :: Signaled
       , runAs :: forall a. internal a -> external (Promise a)
       }

-- Note that the `Signaled` passed to this method MUST be driven by
-- a correctly witnessing object. It is only decoupled from `driver`
-- for convenience to allow forked and unforked drivers. Be careful!
{-# INLINE constructAs #-}
constructAs :: ( Monad internal, MonadIO internal
               , Monad external, MonadIO external
               )
            => Signaled
            -> Signal' (Narrative internalSelf internal) (Narrative internalSelf internal ())
            -> Narrative internalSelf internal `As` external
constructAs buf sig = As buf $ \nar -> liftIO $ do
  p <- newPromiseIO
  bufferIO buf sig $ nar >>= void . fulfill p
  return p

reconstructAs :: forall internal external external' super internalSelf.
                 ( Monad internal, MonadIO internal
                 , Monad external, MonadIO external
                 , Monad external', MonadIO external'
                 , Monad super, MonadIO super
                 )
              => Narrative internalSelf internal `As` external -> super (Narrative internalSelf internal `As` external')
reconstructAs (As buf _) = do
  sig :: Signal internalSelf internal (Narrative internalSelf internal ()) <- runner
  return $ As buf $ \nar -> liftIO $ do
    p <- newPromiseIO
    bufferIO buf sig $ nar >>= void . fulfill p
    return p

{-# INLINE newSignalBuffer #-}
newSignalBuffer :: (Monad super, MonadIO super) => super Signaled
newSignalBuffer = Signaled <$> liftIO newQueueIO

{-# INLINE driver #-}
driver :: (Monad super, MonadIO super, MonadThrow super, Ma (Traits traits) (Messages self))
       => Signaled -> Object traits super -> super ()
driver (Signaled buf) = go
  where

    {-# INLINE go #-}
    go obj = do
      -- re-feed loop to avoid strange stack behavior in browser with ghcjs
      -- shouldn't affect ghc, I think. I assume this has something to do with
      -- gc but I haven't looked.
      (obj',_) <- obj $. go'
      go obj'
      where

        {-# INLINE go' #-}
        go' = do
          evss <- handle (\BlockedIndefinitelyOnSTM -> throwM DriverStopped) $ liftIO (collectIO buf)
          forM_ evss $ \(Signaling evs s) ->
            forM_ evs (signal (unsafeCoerce s))

driverPrintExceptions :: (Monad super, MonadIO super, MonadThrow super, Ma (Traits traits) (Messages self))
                      => String -> Signaled -> Object traits super -> super ()
driverPrintExceptions exceptionPrefix (Signaled buf) = go
  where

    {-# INLINE go #-}
    go obj = do
      (obj',_) <- obj $. go'
      go obj'
      where

        {-# INLINE go' #-}
        go' = do
          evss <- handle (\BlockedIndefinitelyOnSTM -> throwM DriverStopped) $ liftIO (collectIO buf)
          forM_ evss $ \(Signaling evs s) ->
            forM_ evs (handle (\(e :: SomeException) -> liftIO $ putStrLn $ exceptionPrefix ++ ": " ++ show e) . signal (unsafeCoerce s))

data DriverStopped = DriverStopped deriving Show
instance Exception DriverStopped

{-# INLINE buffer #-}
buffer :: (Monad super', MonadIO super')
              => Signaled
              -> Signal' super e
              -> e
              -> super' ()
buffer buf sig e = liftIO $ bufferIO buf sig e

{-# INLINE bufferIO #-}
bufferIO :: Signaled
                -> Signal' super e
                -> e
                -> IO ()
bufferIO (Signaled gb) sig e = arriveIO gb $ Signaling [e] sig
