{-# language BangPatterns #-}
module Ef.Event where

import Ef

import Data.Queue
import Data.Promise

import Control.Concurrent
import Control.Exception
import Data.IORef
import Data.Maybe
import Unsafe.Coerce

-- This module is simultaneously the most used and most despised. It will
-- eventually disappear.

-- A highly simplified implementation of reactive programming; behaviors
-- are in an Event monad that permits behavior self-modification including
-- short-circuiting, exit, and switching. Signals are a sequence of mutable
-- behaviors.

-- NOTE: To avoid bugs, do not share signals, networks, or periodicals
--       across thread boundaries. Signaling/syndicating/publishing,
--       respectively, should be managed by the context in which the
--       event primitive was created. Similarly, adding
--       behaviors/periodicals/subscriptions, respectively, should be
--       done in a synchronous context in which the event primitive was
--       created. This doesn't seem to be a difficult problem to avoid when
--       using threaded contexts run in event loops with Signaling/Signaled.
--       The issue, ultimately, is the use of IORefs rather than MVars;
--       when one thread is signaling/syndicating/publishing to a large
--       set of behaviors/periodicals/subscriptions and another thread is
--       trying to add a behavior/periodical/subscription, the addition
--       might fail silently when the message dispatch has finished and
--       the resultant modified behaviors are overwritten.

data Event k where
  Become :: (e -> Code '[Event] c ()) -> k -> Event k
  Continue :: Event k
  End :: Event k

instance Functor Event where
  fmap f (Become ec k) = Become ec (f k)
  fmap f Continue = Continue
  fmap f End = End

become :: Monad c => (e -> Code '[Event] c ()) -> Code '[Event] c ()
become f = Send $ Become f (Return ())

continue :: Monad c => Code '[Event] c a
continue = Send Continue

end :: Monad c => Code '[Event] c a
end = Send End

type Signal ms c e = Signal' (Narrative (Messages ms) c) e

-- NOTE: Because this is implemented as IORef, it is not thread safe;
--       as long as Signals are maintained by a synchronous context
--       they can be assumed safe - as soon as a Signal crosses a
--       thread context boundary, safety is thrown out the window, i.e.
--       adding an e to a Signal could silently fail if the Signal
--       is currently being 'siganl'ed. MVar will eventaully fix this
--       at the cost of losing subsignal.
data Signal' (c :: * -> *) (e :: *)
    = Signal
        {-# UNPACK #-} !(IORef [IORef (e -> Code '[Event] c ())])
    deriving Eq

{-# INLINE behaviorCount #-}
behaviorCount :: (MonadIO c')
              => Signal' c e
              -> c' Int
behaviorCount (Signal bs_) = do
  bs <- liftIO $ readIORef bs_
  return $ length bs

{-# INLINE nullSignal #-}
nullSignal :: (MonadIO c')
           => Signal' c e
           -> c' Bool
nullSignal (Signal bs_) = do
  bs <- liftIO $ readIORef bs_
  return (null bs)

type Behavior ms c e = Behavior' (Narrative (Messages ms) c) e

data Behavior' c e
  = Behavior
      {-# UNPACK #-} !(IORef (e -> Code '[Event] c ()))
  deriving Eq

{-# INLINE construct #-}
construct :: (MonadIO c')
          => c' (Signal' c e)
construct = liftIO $ do
  behaviors <- newIORef []
  return $ Signal behaviors

{-# INLINE constructSelf #-}
constructSelf :: (MonadIO c, Functor (Messages ms)) => Code ms c (Signal ms c e)
constructSelf = construct

-- Slightly more specific than necessary to avoid required type signatures.
{-# INLINE runner #-}
runner :: forall ms c c'.
          (MonadIO c, Monad c')
       => c (Signal' c' (c' ()))
runner = liftIO $ do
  bhvr <- newIORef lift
  behaviors <- newIORef $ [bhvr]
  return $ Signal behaviors

{-# INLINE behavior #-}
behavior :: (MonadIO c')
         => Signal' c e
         -> (e -> Code '[Event] c ())
         -> c' (Behavior' c e)
behavior sig@(Signal behaviors) newBehavior = liftIO $ do
  b <- newIORef newBehavior
  modifyIORef behaviors (++ [b])
  return (Behavior b)

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
mergeS sig0 sig1 = do
  sig <- construct
  bt0 <- behavior sig0 $ lift . signal sig
  bt1 <- behavior sig1 $ lift . signal sig
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
mapS :: ( MonadIO c
        , MonadIO c'
        )
     => Signal' c e
     -> (e -> c e')
     -> c' ( Signal' c e'
           , Behavior' c e
           )
mapS sig f = do
  sig' <- construct
  bt   <- behavior sig $ \e -> lift $ do
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
filterS :: ( MonadIO c
           , MonadIO c'
           )
        => Signal' c e
        -> (e -> c (Maybe e'))
        -> c' ( Signal' c e'
              , Behavior' c e
              )
filterS sig f = do
  sig' <- construct
  bt   <- behavior sig $ \e -> lift $ do
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
duplicate :: (MonadIO c')
          => Behavior' c e
          -> Signal' c e
          -> c' ()
duplicate (Behavior b) (Signal bs_) = liftIO $ modifyIORef bs_ (++ [b])

-- stop is a delayed effect that doesn't happen until the next event, but all
-- internally maintained references are released; stopping an un-needed behavior
-- can permit GC external to the behavior itms.
{-# INLINE stop #-}
stop :: (Monad c, MonadIO c')
     => Behavior' c a -> c' ()
stop (Behavior b_) = liftIO $ atomicModifyIORef' b_ $ const (const end,())

data Runnable c where
  Runnable :: {-# UNPACK #-} !(IORef [(IORef (e -> Code '[Event] c ()))])
           -> {-# UNPACK #-} !(IORef (e -> Code '[Event] c ()))
           -> Code '[Event] c ()
           -> Runnable c

{-# INLINE signal #-}
signal :: forall c e.
          (MonadIO c)
       => Signal' c e
       -> e
       -> c ()
signal sig e = do
  let Signal bs_ = sig
  bs <- liftIO $ readIORef bs_
  seeded <- forM bs $ \f_ -> do
    f <- liftIO $ readIORef f_
    return $ Runnable bs_ f_ (f e)
  signal_ seeded

{-# INLINE signal_ #-}
signal_ :: forall c.
           (MonadIO c)
        => [Runnable c] -> c ()
signal_ [] = return ()
signal_ (r@(Runnable bs_ f_ f):rs) = start rs r
  where
    start :: [Runnable c] -> Runnable c -> c ()
    start rs (Runnable bs_ f_ f) = go' f
      where
        go' :: Code '[Event] c ()
            -> c ()
        go' (Return _) = signal_ rs
        go' (Lift sup) = sup >>= go'
        go' (Do msg) =
          case prj msg of
            ~(Just x) ->
              case x of
                Become f' k -> do
                  liftIO $ writeIORef f_ $ unsafeCoerce f'
                  go' k
                Continue -> signal_ rs
                End -> do
                  liftIO $ do
                  -- in case we arrived here through trigger, nullify the behavior.
                    writeIORef f_ (const end)
                    atomicModifyIORef' bs_ $ \bs ->
                      (Prelude.filter (/= f_) bs,())
                  signal_ rs

{-# INLINE trigger #-}
trigger :: (MonadIO c)
        => Behavior' c e
        -> e
        -> c ()
trigger (Behavior b_) e = do
  b <- liftIO $ readIORef b_
  bs_ <- liftIO $ newIORef []
  signal_ [Runnable bs_ b_ (b e)]

-- NOTE: Because this is implemented as IORef, it is not thread safe;
--       as long as Periodicals are maintained by a synchronous context
--       they can be assumed safe - as soon as a Periodical crosses a
--       thread context boundary, safety is thrown out the window, i.e.
--       adding a subscription to a Periodical could silently fail if the
--       periodical is currently being published. MVar will eventually
--       fix this at the cost of losing subpublish.
data Periodical' c e
  = Periodical {-# UNPACK #-} !(IORef (Maybe (Signal' c e)))
  deriving Eq

type Periodical ms c e = Periodical' (Narrative (Messages ms) c) e

type Subscription' c e = Behavior' c e

type Subscription ms c e = Behavior ms c e

periodical :: (MonadIO c)
             => c (Periodical' c' e)
periodical = do
  sig <- construct
  p <- liftIO $ newIORef (Just sig)
  return $ Periodical p

periodical' :: (MonadIO c)
             => e -> c (Periodical' c' e)
periodical' ev = do
  sig <- construct
  p <- liftIO $ newIORef (Just sig)
  return $ Periodical p

nullPeriodical :: (MonadIO c)
               => Periodical' c' e -> c Bool
nullPeriodical (Periodical p_) = do
  mp <- liftIO $ readIORef p_
  return $ isNothing mp

emptyPeriodical :: (MonadIO c)
                => Periodical' c e -> c Bool
emptyPeriodical (Periodical p_) = do
  mp <- liftIO $ readIORef p_
  maybe (return True) nullSignal mp

subscribe :: (MonadIO c')
          => Periodical' c e
          -> (e -> Code '[Event] c ())
          -> c' (Maybe (Subscription' c e))
subscribe (Periodical p_) f = do
  mp <- liftIO $ readIORef p_
  forM mp $ \sig -> behavior sig f

-- Lost this to improve space performance of periodicals; they kept the last message around and
-- prevented GC to be able to react to a current message/last message on subscription.
-- -- like subscribe, but immediately trigger the behavior with the current value
-- subscribe' :: (MonadIO c)
--            => Periodical' c e
--            -> (e -> Code '[Event] c ())
--            -> c (Maybe (Subscription' c event,Bool))
-- subscribe' (Periodical p_) f = do
--   mp <- liftIO $ readIORef p_
--   forM mp $ \(cur_,sig) -> do
--     bt <- behavior sig f
--     mcur <- liftIO $ readIORef cur_
--     case mcur of
--       Nothing -> return (bt,False)
--       Just cur -> do
--         trigger bt cur
--         return (bt,True)

publish :: (MonadIO c)
        => Periodical' c e
        -> e
        -> c Bool
publish (Periodical p_) ev = do
  mp <- liftIO $ readIORef p_
  case mp of
    Nothing -> return False
    Just sig -> do
      signal sig ev
      return True

-- Lost this to improve space performance of periodicals; it prevented GC of one message
-- per periodical.
-- periodicalCurrent :: (MonadIO c)
--                   => Periodical' c e -> c (Maybe e)
-- periodicalCurrent (Periodical p_) = liftIO $ do
--   mp <- readIORef p_
--   case mp of
--     Just _ -> readIORef cur_
--     _ -> return Nothing

data Syndicated e where
  Syndicated :: Periodical' c e -> Signaled -> Syndicated e

-- syndicated periodical network; periodicals that share a common event;
-- e propagation network for concurrent dispatch.
-- NOTE: Suffer from the same desynchronization issues...
data Network e
  = Network (MVar [Syndicated e])
  deriving Eq

-- create a new syndication network
network :: (MonadIO c)
        => c (Network e)
network = liftIO $ do
  nw <- newMVar []
  return $ Network nw

-- create a new syndication network
network' :: (MonadIO c)
        => e -> c (Network e)
network' ev = liftIO $ do
  nw <- newMVar []
  return $ Network nw

nullNetwork :: (MonadIO c)
            => Network e -> c Bool
nullNetwork (Network nw) = liftIO $ do
  ss <- readMVar nw
  return (null ss)

-- syndicate an e across multiple periodicals in a network
syndicate :: (MonadIO c)
          => Network e -> e -> c ()
syndicate (Network nw) ev = liftIO $ do
  -- garbage collect null periodicals and get the current set of
  -- executable periodicals. The garbage collection is for periodical
  -- that have been nullified but not removed via leaveNetwork. This is
  -- useful for periodicals that are members of multiple networks.
  ss <- modifyMVar nw $ \xs -> do
    mss <- forM xs $ \s@(Syndicated (Periodical p_) sigd) -> do
             mp <- readIORef p_
             case mp of
               Nothing -> return Nothing
               Just _ -> return $ Just s
    let ss' = catMaybes mss
    return (ss',ss')
  forM_ ss $ \s@(Syndicated (Periodical p_) sigd) -> do
    mp <- readIORef p_
    forM_ mp $ \sig ->
      buffer sigd sig ev

-- Lost this to improve GC.
-- networkCurrent :: (MonadIO c) => Network e -> c (Maybe e)
-- networkCurrent (Network nw) = liftIO $ fst <$> readMVar nw

joinNetwork :: (MonadIO c)
            => Network e -> Periodical' c' e -> Signaled -> c ()
joinNetwork (Network nw) pl sg = do
  liftIO $ modifyMVar nw $ \ss -> return (ss ++ [Syndicated pl sg],())

-- Lost this to improve GC.
-- joinNetwork' :: (MonadIO c)
--              => Network e -> Periodical' c' e -> Signaled -> c Bool
-- joinNetwork' ntw@(Network nw) pl@(Periodical p_) sigd = liftIO $ do
--   mp <- readIORef p_
--   case mp of
--     Nothing -> return False
--     Just (sig) -> do
--       mcur <- modifyMVar nw $ \ss -> return ((ev,ss ++ [Syndicated pl sigd]),ev)
--       case mcur of
--         Nothing -> return False
--         Just cur -> do
--           writeIORef cur_ (Just cur)
--           bufferIO sigd sig cur
--           return True

leaveNetwork :: (MonadIO c)
             => Network e -> Periodical' c' e -> c ()
leaveNetwork (Network nw) pl =
  liftIO $
    -- let's hope this works.... if not, just create a counter token or make
    -- Network (IORef [IORef (Maybe (Syndicated e))]) or something
    modifyMVar nw $ \ss ->
      let ss' = filter (\(Syndicated pl' _) -> pl /= unsafeCoerce pl') ss
      in return (ss',())

-- An abstract Signal queue. Useful for building e loops.
-- Note that there is still a need to call unsafeCoerce on the
-- Signal itms since this data type avoids having `ms` and
-- `c` as type variables. It is the responsibility of the
-- programmer to know how to use this safely; single-responsibility
-- for both injection and extraction with unified typing is required.
data Signaling where
    Signaling :: [e] -> {-# UNPACK #-} !(Signal' c e) -> Signaling
data Signaled where
    Signaled :: {-# UNPACK #-} !(IORef (Maybe (Queue Signaling))) -> Signaled

data As internal external
  = As { signaledAs :: Signaled
       , runAs :: forall a. internal a -> external (Promise a)
       }

runAs_ :: Functor external => As internal external -> internal a -> external ()
runAs_ as ia = void $ runAs as ia

runAsIO :: (MonadIO c) => As internal IO -> internal a -> c (Promise a)
runAsIO as f = liftIO $ runAs as f

runAsIO_ :: (MonadIO c) => As internal IO -> internal a -> c ()
runAsIO_ as f = void $ liftIO $ runAs as f

-- Note that the `Signaled` passed to this method MUST be driven by
-- a correctly witnessing object. It is only decoupled from `driver`
-- for convenience to allow forked and unforked drivers. Be careful!
{-# INLINE constructAs #-}
constructAs :: ( MonadIO internal
               , MonadIO external
               , Functor (Messages internalMs)
               )
            => Signaled
            -> Signal' (Narrative (Messages internalMs) internal) (Code internalMs internal ())
            -> Narrative (Messages internalMs) internal `As` external
constructAs buf sig = As buf $ \nar -> liftIO $ do
  p <- promise
  buffer buf sig $ nar >>= void . fulfill p
  return p

reconstructAs :: forall internal external external' c internalMs.
                 ( MonadIO internal
                 , MonadIO external
                 , MonadIO external'
                 , MonadIO c
                 , Functor (Messages internalMs)
                 )
              => Narrative (Messages internalMs) internal `As` external -> c (Narrative (Messages internalMs) internal `As` external')
reconstructAs (As buf _) = do
  sig :: Signal internalMs internal (Code internalMs internal ()) <- runner
  return $ As buf $ \nar -> liftIO $ do
    p <- promise
    buffer buf sig $ nar >>= void . fulfill p
    return p

{-# INLINE newSignalBuffer #-}
newSignalBuffer :: (MonadIO c) => c Signaled
newSignalBuffer = Signaled <$> liftIO (newIORef . Just =<< newQueue)

{-# INLINE driver #-}
driver :: (MonadIO c, Functor (Messages ms), Delta (Modules ts) (Messages ms))
       => Signaled -> Object ts c -> c ()
driver (Signaled buf) o = do
  -- driver is the only place that will hold onto the (Queue Signaling); if the thread is stopped,
  -- which is the only expected time for a Signaled to be nullified, GC won't be prevented.
  Just qs <- liftIO $ readIORef buf
  start qs o
  where
    start qs = go
      where

        go obj = do
          -- re-feed loop to avoid strange stack behavior in browser with ghcjs
          -- shouldn't affect ghc, I think. I assume this has something to do with
          -- gc but I haven't looked.
          (obj',r) <- obj ! go'
          case r of
            Nothing -> return ()
            Just _  -> go obj'
          where

            go' = do
              ms <- liftIO $ handle (\(_ :: SomeException) -> liftIO (writeIORef buf Nothing) >> return Nothing)
                                    (Just <$> collect qs)
              forM ms $ \(Signaling evs s) -> forM_ evs (signal $ unsafeCoerce s)

driverPrintExceptions :: (MonadIO c, Functor (Messages ms), Delta (Modules ts) (Messages ms))
                      => String -> Signaled -> Object ts c -> c ()
driverPrintExceptions exceptionPrefix (Signaled buf) o = do
  Just qs <- liftIO $ readIORef buf
  start qs o
  where
    start qs = go
      where

        go obj = do
          (obj',r) <- obj ! go'
          case r of
            Nothing -> return ()
            Just _  -> go obj'
          where

            -- if the buffer is nullified via killBuffer, a BlockedIndefinitelyOnSTM should eventually be thrown.
            go' = do
              ms <- liftIO $ handle (\(e :: SomeException) -> do
                                        putStrLn $ exceptionPrefix ++ ": " ++ show e
                                        writeIORef buf Nothing
                                        return Nothing
                                    )
                                    (Just <$> collect qs)
              forM ms $ \(Signaling evs s) ->
                forM_ evs (signal $ unsafeCoerce s)

data DriverStopped = DriverStopped deriving Show
instance Exception DriverStopped

{-# INLINE buffer #-}
buffer :: (MonadIO c')
              => Signaled
              -> Signal' c e
              -> e
              -> c' ()
buffer (Signaled buf) sig e = liftIO $ do
  mqs <- readIORef buf
  forM_ mqs $ \qs -> arrive qs $ Signaling [e] sig

killBuffer :: (MonadIO c)
           => Signaled
           -> c ()
killBuffer (Signaled buf) = liftIO $ writeIORef buf Nothing
