module Ef.Event
  ( signalRaw
  , Reactor(..)
  , Signal(..), construct, constructRunner
  , Signaled(..), Signaling(..)
  , Event(..), event
  , BehaviorToken(..)
  , clearSignal

  , behavior
  , enact
  , stop
  , signal
  , trigger
  , mergeSignals
  , mapSignal
  , filterSignal

  , DriverStopped(..)
  , newSignalBuffer
  , buffer, bufferIO
  , As(..), constructAs, reconstructAs
  , driver, driverPrintExceptions
  ) where

import Ef
import Ef.Narrative
import Ef.Bidir

import Data.Queue
import Data.Promise

import Control.Monad
import Data.IORef
import Data.List
import Unsafe.Coerce

import Control.Exception (BlockedIndefinitelyOnSTM(..))

data Reactor self super event =
    Reactor
        {
          become
              :: (event -> Narrative self super ())
              -> Narrative self super ()

        , end
              :: Narrative self super ()

        }

data Signal self super event
    = Signal (IORef (Maybe event)) (IORef Int) (IORef [(Int,event -> Narrative self super ())])
    deriving Eq


-- An abstract Signal queue. Useful for building event loops.
-- Note that there is still a need to call unsafeCoerce on the
-- Signal itself since this data type avoids having `self` and
-- `super` as type variables. It is the responsibility of the
-- programmer to know how to use this safely; single-responsibility
-- for both injection and extraction with unified typing is required.
data Signaling where
    Signaling :: [e] -> Signal self super e -> Signaling
data Signaled where
    Signaled :: Queue Signaling -> Signaled

-- | Construct a new signal with an initial value. Undefined is a valid
-- start value as long as any behaviors linked to the `Signal` are not
-- linked via `behavior'`.
construct :: (Monad super, MonadIO super)
          => Maybe event -> super (Signal self super' event)
construct mevent = do
    current   <- liftIO $ newIORef mevent
    count     <- liftIO $ newIORef 0
    behaviors <- liftIO $ newIORef []
    return $ Signal current count behaviors
{-# INLINE construct #-}

constructRunner :: (Monad super, MonadIO super, Monad super')
                 => super (Signal self super' (Narrative self super' ()))
constructRunner  = do
  current   <- liftIO $ newIORef (Just $ return ())
  count     <- liftIO $ newIORef 0
  behaviors <- liftIO $ newIORef [(-1,id)]
  return $ Signal current count behaviors

clearSignal :: (Monad super, MonadIO super)
            => Signal self super' e -> super ()
clearSignal (Signal _ _ bs) = liftIO $ writeIORef bs []
{-# INLINE clearSignal #-}

newBehaviorId :: forall self super super' event.
                ( Monad super, MonadIO super
                , Monad super', MonadIO super'
                )
              => Signal self super event
              -> super' Int
newBehaviorId (Signal _ c_ _) = do
  liftIO $ atomicModifyIORef c_ $ \c ->
    let c' = c + 1
    in c' `seq` (c',c)

merge__ :: forall self super event. (Monad super, MonadIO super)
       => (Signal self super event -> event -> Narrative self super ())
       -> Signal self super event
       -> Signal self super event
       -> Narrative self super ( Signal self super event
                               , BehaviorToken self super event
                               , BehaviorToken self super event
                               , BehaviorToken self super event
                               )
merge__ signalMethod sig0 sig1 = do
    sig <- construct Nothing
    bt  <- behavior__ sig  (signalMethod sig)
    bt0 <- behavior__ sig0 (signalMethod sig)
    bt1 <- behavior__ sig1 (signalMethod sig)
    return (sig,bt0,bt1,bt)
{-# INLINE merge__ #-}

readCurrent :: forall self super event super'.
               ( Monad super, MonadIO super
               , Monad super', MonadIO super'
               )
            => Signal self super event
            -> super' (Maybe event)
readCurrent (Signal current _ _) = liftIO $ readIORef current

combine__ :: forall self super event1 event2. (Monad super, MonadIO super)
          => (forall e. Signal self super e -> e -> Narrative self super ())
          -> Signal self super event1
          -> Signal self super event2
          -> Narrative self super ( Signal self super (Maybe event1,Maybe event2)
                                  , BehaviorToken self super event1
                                  , BehaviorToken self super event2
                                  , BehaviorToken self super (Maybe event1,Maybe event2)
                                  )
combine__ signalMethod sig0 sig1 = do
  sig <- construct Nothing
  bt  <- behavior__ sig (signalMethod sig)
  bt0 <- behavior__ sig0 (\e0 -> do
                      c1 <- readCurrent sig1
                      signalMethod sig (e0,c1)
                    )
  bt1 <- behavior__ sig1 (\e1 -> do
                      c0 <- readCurrent sig1
                      signalMethod sig (c0,e1)
                    )
  return (sig,bt0,bt1,bt)

mapSignal__ :: (Monad super, MonadIO super)
           => (Signal self super b -> b -> Narrative self super ())
           -> (a -> b)
           -> Signal self super a
           -> Narrative self super ( Signal self super b
                                   , BehaviorToken self super a
                                   , BehaviorToken self super b
                                   )
mapSignal__ signalMethod f sig@(Signal current0 count0 behaviors0) = do
    signal <- construct Nothing
    bt <- behavior__ sig (signalMethod signal . f)
    return (signal,bt)
{-# INLINE mapSignal__ #-}

filterSignal__ :: (Monad super, MonadIO super)
              => (Signal self super b -> b -> Narrative self super ())
              -> (a -> Maybe b)
              -> Signal self super a
              -> Narrative self super ( Signal self super b
                                      , BehaviorToken self super a
                                      , BehaviorToken self super b)
filterSignal__ signalMethod predicate sig@(Signal _ count0 behaviors0) = do
    signal <- construct Nothing
    let newBehavior event =
          when (predicate event) $ signalMethod signal event
    bt <- behavior__ sig newBehavior
    return (signal,bt)
{-# INLINE filterSignal__ #-}

behavior__ :: (Monad super, MonadIO super)
          => Signal self super event
          -> (event -> Narrative self super ())
          -> Narrative self super (BehaviorToken self super event)
behavior__ sig@(Signal _ count behaviors) newBehavior = do
    c <- liftIO $ atomicModifyIORef count $ \c -> (c + 1,c)
    liftIO $ modifyIORef behaviors (++ [(c,newBehavior)])
    return (BehaviorToken c sig)
{-# INLINE behavior__ #-}

stop :: (Monad super, MonadIO super)
     => BehaviorToken self' super' event
     -> Narrative self super ()
stop (BehaviorToken bt (Signal _ _ behaviors)) =
    liftIO $ modifyIORef behaviors $ filter ((/=) bt . fst)
{-# INLINE stop #-}

signalRaw :: (Monad super, MonadIO super)
        => Signal self super event
        -> event
        -> Narrative self super ()
signalRaw (Signal current count behaviors) event = do
    writeCurrent <- liftIO $ writeIORef current event
    bs <- liftIO $ readIORef behaviors
    let applied = map (($ event) . snd) bs
    sequence_ applied
{-# INLINE signalRaw #-}

trigger__ :: (Monad super, MonadIO super)
         => BehaviorToken self super event
         -> event
         -> Narrative self super ()
trigger__ (BehaviorToken bt (Signal current count behaviors)) event = do
    bs <- liftIO $ readIORef behaviors
    let (eq,neq) = partition ((==) bt . fst) bs
    case eq of
        [x] -> snd x event
        [] -> return ()
{-# INLINE trigger__ #-}

data BehaviorToken self super event = BehaviorToken Int (Signal self super event)
  deriving Eq

data Event self super =
    Event
        { trigger_
              :: forall event.
                 BehaviorToken self super event
              -> event
              -> Narrative self super ()

          -- | Signal an event on a signal; behaviors connected to the signal
          -- will be run in place.
        , signal_
              :: forall event.
                 Signal self super event
              -> event
              -> Narrative self super ()

        , enact_
              :: forall event.
                 BehaviorToken self super event
              -> Narrative self super ()

          -- | Create a new reactive behavior connected to that signal.
          -- The behaviors are given a `Reactor` interface to permit statefulness
          -- and self-death.
        , behavior_
              :: forall event.
                 Signal self super event
              -> (Reactor self super event -> event -> Narrative self super ())
              -> Narrative self super (BehaviorToken self super event)

          -- | merge two `Signal`s; any time either `Signal` is `signal`ed, the
          -- new signal will be signaled with that event.
        , mergeSignals_
              :: forall event.
                 Signal self super event
              -> Signal self super event
              -> Narrative self super ( Signal self super event
                                      , BehaviorToken self super event
                                      , BehaviorToken self super event
                                      )

        , combineSignals_
              :: forall event event'.
                 Signal self super event
              -> Signal self super event'
              -> Narrative self super ( Signal self super (Maybe event,Maybe event')
                                      , BehaviorToken self super event
                                      , BehaviorToken self super event'
                                      , BehaviorToken self super (Maybe event,Maybe event')
                                      )


          -- | create a new `Signal` by mapping a function over an existing `Signal`.
        , mapSignal_
              :: forall a b.
                 (a -> b)
              -> Signal self super a
              -> Narrative self super ( Signal self super b
                                      , BehaviorToken self super b
                                      )

          -- | create a new `Signal` by filtering an existing `Signal` with a predicative function.
        , filterSignal_
              :: forall a b.
                 (a -> Maybe b)
              -> Signal self super a
              -> Narrative self super ( Signal self super b
                                      , BehaviorToken self super b
                                      )
        }

data Action self super
    where

        Trigger
            :: (    Action self super
                 -> Narrative self super (Narrative self super ())
               )
            -> BehaviorToken self super event
            -> event
            -> Action self super

        Signal_
            :: (    Action self super
                 -> Narrative self super (Narrative self super ())
               )
            -> Signal self super event
            -> event
            -> Action self super

        Enact
            :: (    Action self super
                 -> Narrative self super (Narrative self super ())
               )
            -> BehaviorToken self super event
            -> Action self super

        Continue
            :: Action self super

        Become
            :: (event -> Narrative self super ())
            -> Action self super

        End
            :: Action self super

-- | Scope an event framework to work with `Signal`s.
--
-- @
--     let sig1 = construct (0 :: Int)
--         sig2 = construct 'a'
--     event $ \Event{..} -> do
--         behavior sig1 $ \\n -> signal sig2 (toEnum n)
--         behavior sig2 $ \\ch -> do
--             if ch == 'z' then io (putChar '\n') >> end
--                        else io (putChar ch)
--         mapM_ (signal sig1) [1..100]
-- @
event :: forall self super a. (Monad super, MonadIO super, '[Bidir] <: self)
      => (Event self super -> Narrative self super a)
      -> Narrative self super a
event loop =
    let
        ev
            :: (   Action self super
                -> Narrative self super (Narrative self super ())
               )
            -> Event self super

        ev up =
            Event { trigger_ =
                        \bt event ->
                            join $ up (Trigger up bt event)

                  , signal_ =
                        \signal event ->
                            join $ up (Signal_ up signal event)

                  , enact_ =
                        \bt ->
                            join $ up (Enact up bt)

                  , behavior_ =
                        \signal b ->
                            let reactor = Reactor
                                    { become =
                                          \newBehavior ->
                                              join $ up (Become newBehavior)
                                    , end = join $ up End
                                    }
                            in behavior__ signal (b reactor)

                  , mergeSignals_ =
                        merge__ (\signal event -> join $ up (Signal_ up signal event))

                  , combineSignals_ =
                        combine__ (\signal event -> join $ up (Signal_ up signal event))

                  , mapSignal_ =
                        mapSignal__
                            (\signal event -> join $ up (Signal_ up signal event))

                  , filterSignal_ =
                        filterSignal__
                            (\signal event -> join $ up (Signal_ up signal event))
                  }
    in runBidir $ server +>> (knotted $ \up _ -> loop (ev up))
    where

        server :: forall x. Action self super -> Bi X () (Action self super) (Narrative self super ()) self super x
        server initialRequest =
            knotted $ \_ dn -> withRespond dn initialRequest
            where

                withRespond
                    :: (    Narrative self super ()
                         -> Narrative self super (Action self super)
                       )
                    -> Action self super
                    -> Narrative self super x

                withRespond respond =
                    eventLoop
                    where

                        eventLoop :: Action self super -> Narrative self super x
                        eventLoop req =
                            case req of

                                Signal_ up (Signal currentRef _ behaviorsRef) event -> do
                                    behaviors <- liftIO $ readIORef behaviorsRef
                                    liftIO $ writeIORef currentRef event
                                    newBehaviors <- runBehaviors up event behaviors
                                    liftIO $ writeIORef behaviorsRef newBehaviors
                                    newRequest <- respond (return ())
                                    eventLoop newRequest

                                Enact up (BehaviorToken bt (Signal currentRef _ behaviorsRef)) -> do
                                    behaviors <- liftIO $ readIORef behaviorsRef
                                    let (eq,neq) = partition ((==) bt . fst) behaviors -- will be ([] or [x],xs)
                                    current <- liftIO $ readIORef currentRef
                                    newBehaviors <- runBehaviors up current eq
                                    liftIO $ writeIORef behaviorsRef (newBehaviors ++ neq)
                                    newRequest <- respond (return ())
                                    eventLoop newRequest

                                Trigger up (BehaviorToken bt (Signal _ _ behaviorsRef)) event -> do
                                    behaviors <- liftIO $ readIORef behaviorsRef
                                    let (eq,neq) = partition ((==) bt . fst) behaviors
                                    newBehaviors <- runBehaviors up event eq
                                    liftIO $ writeIORef behaviorsRef (newBehaviors ++ neq)
                                    newRequest <- respond (return ())
                                    eventLoop newRequest

                        runBehaviors
                            :: forall event.
                               (    Action self super
                                 -> Narrative self super (Narrative self super ())
                               )
                            -> event
                            -> [(Int,event -> Narrative self super ())]
                            -> Narrative self super [(Int,event -> Narrative self super ())]

                        runBehaviors up event =
                            withAcc []
                            where

                                withAcc acc [] = return (reverse acc)

                                withAcc acc (behavior0:behaviors) = do
                                    newRequest <- respond $ do snd behavior0 event
                                                               join (up Continue)
                                    withBehavior behavior0 True newRequest
                                    where

                                        withBehavior (bt,behavior) alive request =
                                            case request of

                                                Signal_ up' (Signal currentRef _ behaviorsRef) event' -> do
                                                    behaviors' <- liftIO $ readIORef behaviorsRef
                                                    liftIO $ writeIORef currentRef event'
                                                    newBehaviors <- runBehaviors up' event' behaviors'
                                                    liftIO $ writeIORef behaviorsRef newBehaviors
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,behavior) alive newRequest

                                                Enact up' (BehaviorToken bt (Signal currentRef _ behaviorsRef)) -> do
                                                    behaviors <- liftIO $ readIORef behaviorsRef
                                                    let (eq,neq) = partition ((==) bt . fst) behaviors -- will be ([] or [x],xs)
                                                    current <- liftIO $ readIORef currentRef
                                                    newBehaviors <- runBehaviors up current eq
                                                    liftIO $ writeIORef behaviorsRef (newBehaviors ++ neq)
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,behavior) alive newRequest

                                                Trigger up (BehaviorToken bt' (Signal _ _ behaviorsRef)) event -> do
                                                    behaviors <- liftIO $ readIORef behaviorsRef
                                                    let (eq,neq) = partition ((==) bt' . fst) behaviors
                                                    newBehaviors <- runBehaviors up event eq
                                                    liftIO $ writeIORef behaviorsRef (newBehaviors ++ neq)
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,behavior) alive newRequest

                                                Become f -> do
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,unsafeCoerce f) alive newRequest

                                                End -> do
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,behavior) False newRequest

                                                Continue ->
                                                    if alive then
                                                        withAcc ((bt,behavior):acc) behaviors
                                                    else
                                                        withAcc acc behaviors
{-# INLINE event #-}

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
            -> Signal internalSelf internal (Narrative internalSelf internal ())
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
  sig :: Signal internalSelf internal (Narrative internalSelf internal ()) <- constructRunner
  return $ As buf $ \nar -> liftIO $ do
    p <- newPromiseIO
    bufferIO buf sig $ nar >>= void . fulfill p
    return p

{-# INLINE newSignalBuffer #-}
newSignalBuffer :: (Monad super, MonadIO super) => super Signaled
newSignalBuffer = Signaled <$> liftIO newQueueIO

{-# INLINE driver #-}
driver :: (Monad super, MonadIO super, MonadThrow super, Ma (Traits traits) (Messages self), '[Bidir] <: self)
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

driverPrintExceptions :: (Monad super, MonadIO super, MonadThrow super, Ma (Traits traits) (Messages self), '[Bidir] <: self)
                      => String -> Signaled -> Object traits super -> super ()
driverPrintExceptions exceptionPrefix (Signaled buf) = go
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
            forM_ evs (handle (\(e :: SomeException) -> liftIO $ putStrLn $ exceptionPrefix ++ ": " ++ show e) . signal (unsafeCoerce s))

data DriverStopped = DriverStopped deriving Show
instance Exception DriverStopped

{-# INLINE buffer #-}
buffer :: (Monad super', MonadIO super')
              => Signaled
              -> Signal self super e
              -> e
              -> Narrative self' super' ()
buffer buf sig e = liftIO $ bufferIO buf sig e

{-# INLINE bufferIO #-}
bufferIO :: Signaled
                -> Signal self super e
                -> e
                -> IO ()
bufferIO (Signaled gb) sig e = arriveIO gb $ Signaling [e] sig

{-# INLINE behavior #-}
behavior :: ('[Bidir] <: self, Monad super, MonadIO super)
         => Signal self super e
         -> (Reactor self super e -> e -> Narrative self super ())
         -> Narrative self super (BehaviorToken self super e)
behavior sig b = event $ \e -> behavior_ e sig b

{-# INLINE enact #-}
enact :: ('[Bidir] <: self, Monad super, MonadIO super)
      => BehaviorToken self super e
      -> Narrative self super ()
enact bt = event $ \e -> enact_ e bt

{-# INLINE signal #-}
signal :: ('[Bidir] <: self, Monad super, MonadIO super)
       => Signal self super e
       -> e
       -> Narrative self super ()
signal sig ev =
    event $ \e ->
        signal_ e sig ev

{-# INLINE trigger #-}
trigger :: ('[Bidir] <: self, Monad super, MonadIO super)
        => BehaviorToken self super e
        -> e
        -> Narrative self super ()
trigger bt ev =
    event $ \e ->
        trigger_ e bt ev

{-# INLINE mergeSignals #-}
mergeSignals :: ('[Bidir] <: self, Monad super, MonadIO super)
             => Signal self super e
             -> Signal self super e
             -> Narrative self super ( Signal self super e
                                     , BehaviorToken self super e
                                     , BehaviorToken self super e
                                     )
mergeSignals sig1 sig2 = event $ \e ->
    mergeSignals_ e sig1 sig2

{-# INLINE combineSignals #-}
combineSignals :: ('[Bidir] <: self, Monad super, MonadIO super)
               => Signal self super e
               -> Signal self super e'
               -> Narrative self super ( Signal self super (Maybe e,Maybe e')
                                       , BehaviorToken self super e
                                       , BehaviorToken self super e'
                                       , BehaviorToken self super (Maybe e,Maybe e')
                                       )
combineSignals sig1 sig2 = event $ \e ->
  combineSignals_ e sig1 sig2

{-# INLINE mapSignal #-}
mapSignal :: ('[Bidir] <: self, Monad super, MonadIO super)
          => (e -> e')
          -> Signal self super e
          -> Narrative self super ( Signal self super e'
                                  , BehaviorToken self super e'
                                  )
mapSignal f sig = event $ \e ->
    mapSignal_ e f sig

{-# INLINE filterSignal #-}
filterSignal :: ('[Bidir] <: self, Monad super, MonadIO super)
             => (e -> Maybe e')
             -> Signal self super e
             -> Narrative self super ( Signal self super e'
                                     , BehaviorToken self super e'
                                     )
filterSignal f sig = event $ \e ->
    filterSignal_ e f sig
