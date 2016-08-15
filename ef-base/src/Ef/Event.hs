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
import Ef.IO

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
    = Signal (IORef event) (IORef Int) (IORef [(Int,event -> Narrative self super ())])
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
construct :: (Monad super, Lift IO super)
          => event -> super (Signal self super' event)
construct event = do
    current <- lift $ newIORef event
    count <- lift $ newIORef 0
    behaviors <- lift $ newIORef []
    return $ Signal current count behaviors
{-# INLINE construct #-}

constructRunner :: (Monad super, Lift IO super, Monad super')
                 => super (Signal self super' (Narrative self super' ()))
constructRunner  = do
  current <- lift $ newIORef (return ())
  count <- lift $ newIORef 0
  behaviors <- lift $ newIORef [(-1,id)]
  return $ Signal current count behaviors

clearSignal :: (Monad super, Lift IO super)
            => Signal self super' e -> super ()
clearSignal (Signal _ _ bs) = lift $ writeIORef bs []
{-# INLINE clearSignal #-}

merge__ :: forall self super event. (Monad super, Lift IO super)
       => (Signal self super event -> event -> Narrative self super ())
       -> event
       -> Signal self super event
       -> Signal self super event
       -> Narrative self super (Signal self super event,BehaviorToken self super event, BehaviorToken self super event)
merge__ signalMethod initial sig0@(Signal current0 count0 behaviors0) sig1@(Signal current1 count1 behaviors1) = do
    signal <- construct initial
    c0 <- lift $ atomicModifyIORef count0 $ \c -> (c + 1,c)
    lift $ modifyIORef behaviors0 $ \bs ->
        let newBehavior event = signalMethod signal event
        in bs ++ [(c0,newBehavior)]
    c1 <- lift $ atomicModifyIORef count1 $ \c -> (c + 1,c)
    lift $ modifyIORef behaviors1 $ \bs ->
        let newBehavior event = signalMethod signal event
        in bs ++ [(c1,newBehavior)]
    return (signal,BehaviorToken c0 sig0,BehaviorToken c1 sig1)
{-# INLINE merge__ #-}

mapSignal__ :: (Monad super, Lift IO super)
           => (Signal self super b -> b -> Narrative self super ())
           -> (a -> b)
           -> b
           -> Signal self super a
           -> Narrative self super (Signal self super b, BehaviorToken self super b)
mapSignal__ signalMethod f initial sig@(Signal current0 count0 behaviors0) = do
    signal <- construct initial
    c <- lift $ atomicModifyIORef count0 $ \c -> (c + 1,c)
    lift $ modifyIORef behaviors0 $ \bs ->
        let newBehavior event = signalMethod signal (f event)
        in bs ++ [(c,newBehavior)]
    return (signal,BehaviorToken c signal)
{-# INLINE mapSignal__ #-}

filterSignal__ :: (Monad super, Lift IO super)
              => (Signal self super a -> a -> Narrative self super ())
              -> (a -> Bool)
              -> a
              -> Signal self super a
              -> Narrative self super (Signal self super a,BehaviorToken self super a)
filterSignal__ signalMethod predicate initial sig@(Signal _ count0 behaviors0) = do
    signal <- construct initial
    c <- lift $ atomicModifyIORef count0 $ \c -> (c + 1,c)
    lift $ modifyIORef behaviors0 $ \bs ->
        let newBehavior event =
                if predicate event then
                    signalMethod signal event
                else
                    return ()
        in bs ++ [(c,newBehavior)]
    return (signal,BehaviorToken c signal)
{-# INLINE filterSignal__ #-}

behavior__ :: (Monad super, Lift IO super)
          => Signal self super event
          -> (event -> Narrative self super ())
          -> Narrative self super (BehaviorToken self super event)
behavior__ sig@(Signal _ count behaviors) newBehavior = do
    c <- lift $ atomicModifyIORef count $ \c -> (c + 1,c)
    lift $ modifyIORef behaviors (++ [(c,newBehavior)])
    return (BehaviorToken c sig)
{-# INLINE behavior__ #-}

stop :: (Monad super, Lift IO super)
     => BehaviorToken self' super' event
     -> Narrative self super ()
stop (BehaviorToken bt (Signal _ _ behaviors)) =
    lift $ modifyIORef behaviors $ filter ((/=) bt . fst)
{-# INLINE stop #-}

signalRaw :: (Monad super, Lift IO super)
        => Signal self super event
        -> event
        -> Narrative self super ()
signalRaw (Signal current count behaviors) event = do
    writeCurrent <- lift $ writeIORef current event
    bs <- lift $ readIORef behaviors
    let applied = map (($ event) . snd) bs
    sequence_ applied
{-# INLINE signalRaw #-}

trigger__ :: (Monad super, Lift IO super)
         => BehaviorToken self super event
         -> event
         -> Narrative self super ()
trigger__ (BehaviorToken bt (Signal current count behaviors)) event = do
    bs <- lift $ readIORef behaviors
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
                 event
              -> Signal self super event
              -> Signal self super event
              -> Narrative self super (Signal self super event,BehaviorToken self super event,BehaviorToken self super event)

          -- | create a new `Signal` by mapping a function over an existing `Signal`.
        , mapSignal_
              :: forall a b.
                 (a -> b)
              -> b
              -> Signal self super a
              -> Narrative self super (Signal self super b,BehaviorToken self super b)

          -- | create a new `Signal` by filtering an existing `Signal` with a predicative function.
        , filterSignal_
              :: forall a.
                 (a -> Bool)
              -> a
              -> Signal self super a
              -> Narrative self super (Signal self super a,BehaviorToken self super a)
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
event :: forall self super a. (Monad super, Lift IO super, '[Bidir] <: self)
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

                  , mapSignal_ =
                        mapSignal__
                            (\signal event -> join $ up (Signal_ up signal event))

                  , filterSignal_ =
                        filterSignal__
                            (\signal event -> join $ up (Signal_ up signal event))
                  }
    in runBidir $ server +>> (bi $ \up _ -> loop (ev up))
    where

        server :: forall x. Action self super -> Bi X () (Action self super) (Narrative self super ()) self super x
        server initialRequest =
            bi $ \_ dn -> withRespond dn initialRequest
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
                                    behaviors <- lift $ readIORef behaviorsRef
                                    lift $ writeIORef currentRef event
                                    newBehaviors <- runBehaviors up event behaviors
                                    lift $ writeIORef behaviorsRef newBehaviors
                                    newRequest <- respond (return ())
                                    eventLoop newRequest

                                Enact up (BehaviorToken bt (Signal currentRef _ behaviorsRef)) -> do
                                    behaviors <- lift $ readIORef behaviorsRef
                                    let (eq,neq) = partition ((==) bt . fst) behaviors -- will be ([] or [x],xs)
                                    current <- lift $ readIORef currentRef
                                    newBehaviors <- runBehaviors up current eq
                                    lift $ writeIORef behaviorsRef (newBehaviors ++ neq)
                                    newRequest <- respond (return ())
                                    eventLoop newRequest

                                Trigger up (BehaviorToken bt (Signal _ _ behaviorsRef)) event -> do
                                    behaviors <- lift $ readIORef behaviorsRef
                                    let (eq,neq) = partition ((==) bt . fst) behaviors
                                    newBehaviors <- runBehaviors up event eq
                                    lift $ writeIORef behaviorsRef (newBehaviors ++ neq)
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
                                                    behaviors' <- lift $ readIORef behaviorsRef
                                                    lift $ writeIORef currentRef event'
                                                    newBehaviors <- runBehaviors up' event' behaviors'
                                                    lift $ writeIORef behaviorsRef newBehaviors
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,behavior) alive newRequest

                                                Enact up' (BehaviorToken bt (Signal currentRef _ behaviorsRef)) -> do
                                                    behaviors <- lift $ readIORef behaviorsRef
                                                    let (eq,neq) = partition ((==) bt . fst) behaviors -- will be ([] or [x],xs)
                                                    current <- lift $ readIORef currentRef
                                                    newBehaviors <- runBehaviors up current eq
                                                    lift $ writeIORef behaviorsRef (newBehaviors ++ neq)
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,behavior) alive newRequest

                                                Trigger up (BehaviorToken bt' (Signal _ _ behaviorsRef)) event -> do
                                                    behaviors <- lift $ readIORef behaviorsRef
                                                    let (eq,neq) = partition ((==) bt' . fst) behaviors
                                                    newBehaviors <- runBehaviors up event eq
                                                    lift $ writeIORef behaviorsRef (newBehaviors ++ neq)
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
constructAs :: ( Monad internal, Lift IO internal
               , Monad external, Lift IO external
               )
            => Signaled
            -> Signal internalSelf internal (Narrative internalSelf internal ())
            -> Narrative internalSelf internal `As` external
constructAs buf sig = As buf $ \nar -> liftIO $ do
  p <- newPromiseIO
  bufferIO buf sig $ nar >>= void . fulfill p
  return p

reconstructAs :: forall internal external external' super internalSelf.
                 ( Monad internal, Lift IO internal
                 , Monad external, Lift IO external
                 , Monad external', Lift IO external'
                 , Monad super, Lift IO super
                 )
              => Narrative internalSelf internal `As` external -> super (Narrative internalSelf internal `As` external')
reconstructAs (As buf _) = do
  sig :: Signal internalSelf internal (Narrative internalSelf internal ()) <- constructRunner
  return $ As buf $ \nar -> liftIO $ do
    p <- newPromiseIO
    bufferIO buf sig $ nar >>= void . fulfill p
    return p

{-# INLINE newSignalBuffer #-}
newSignalBuffer :: (Monad super, Lift IO super) => super Signaled
newSignalBuffer = Signaled <$> lift newQueueIO

{-# INLINE driver #-}
driver :: (Monad super, Lift IO super, Ma (Traits traits) (Messages self), '[Bidir] <: self)
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
          evss <- mapException (\BlockedIndefinitelyOnSTM -> DriverStopped) $ io (collectIO buf)
          forM_ evss $ \(Signaling evs s) ->
            forM_ evs (signal (unsafeCoerce s))

driverPrintExceptions :: (Monad super, Lift IO super, Ma (Traits traits) (Messages self), '[Bidir] <: self)
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
          evss <- mapException (\BlockedIndefinitelyOnSTM -> DriverStopped) $ io (collectIO buf)
          forM_ evss $ \(Signaling evs s) ->
            forM_ evs (handle (\(e :: SomeException) -> lift $ putStrLn $ exceptionPrefix ++ ": " ++ show e) . signal (unsafeCoerce s))

data DriverStopped = DriverStopped deriving Show
instance Exception DriverStopped

{-# INLINE buffer #-}
buffer :: (Monad super', Lift IO super')
              => Signaled
              -> Signal self super e
              -> e
              -> Narrative self' super' ()
buffer buf sig e = lift $ bufferIO buf sig e

{-# INLINE bufferIO #-}
bufferIO :: Signaled
                -> Signal self super e
                -> e
                -> IO ()
bufferIO (Signaled gb) sig e = arriveIO gb $ Signaling [e] sig

{-# INLINE behavior #-}
behavior :: ('[Bidir] <: self, Monad super, Lift IO super)
         => Signal self super e
         -> (Reactor self super e -> e -> Narrative self super ())
         -> Narrative self super (BehaviorToken self super e)
behavior sig b = event $ \e -> behavior_ e sig b

{-# INLINE enact #-}
enact :: ('[Bidir] <: self, Monad super, Lift IO super)
      => BehaviorToken self super e
      -> Narrative self super ()
enact bt = event $ \e -> enact_ e bt

{-# INLINE signal #-}
signal :: ('[Bidir] <: self, Monad super, Lift IO super)
       => Signal self super e
       -> e
       -> Narrative self super ()
signal sig ev =
    event $ \e ->
        signal_ e sig ev

{-# INLINE trigger #-}
trigger :: ('[Bidir] <: self, Monad super, Lift IO super)
        => BehaviorToken self super e
        -> e
        -> Narrative self super ()
trigger bt ev =
    event $ \e ->
        trigger_ e bt ev

{-# INLINE mergeSignals #-}
mergeSignals :: ('[Bidir] <: self, Monad super, Lift IO super)
             => Signal self super e
             -> Signal self super e
             -> Narrative self super (Signal self super e,BehaviorToken self super e,BehaviorToken self super e)
mergeSignals sig1 sig2 = event $ \e ->
    mergeSignals_ e undefined sig1 sig2

{-# INLINE mapSignal #-}
mapSignal :: ('[Bidir] <: self, Monad super, Lift IO super)
          => (e -> e')
          -> Signal self super e
          -> Narrative self super (Signal self super e',BehaviorToken self super e')
mapSignal f sig = event $ \e ->
    mapSignal_ e f undefined sig

{-# INLINE filterSignal #-}
filterSignal :: ('[Bidir] <: self, Monad super, Lift IO super)
             => (e -> Bool)
             -> Signal self super e
             -> Narrative self super (Signal self super e,BehaviorToken self super e)
filterSignal f sig = event $ \e ->
    filterSignal_ e f undefined sig
