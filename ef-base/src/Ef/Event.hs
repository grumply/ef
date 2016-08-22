{-# language BangPatterns #-}
module Ef.Event where

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

import qualified Data.IntMap.Strict as Map

data Event k where
  Become :: (event -> Narrative self super ())
         -> k
         -> Event k

  Continue :: Action k

  End :: Action k

  Subsignal :: Signal self super event'
            -> event'
            -> k
            -> Action k

become :: Monad super => Narrative '[Event] (Narrative self super) ()
become = self $ Become f ()

continue :: Monad super => Narrative '[Event] (Narrative self super) a
continue = self Continue

end :: Monad super => Narrative '[Event] (Narrative self super) a
end = self End

subsignal :: Monad super
          => Signal self super event
          -> event
          -> Narrative '[Event] (Narrative self super) ()
subsignal sig e = self $ Subsignal sig e ()

data Signal self super event
    = Signal
        (IORef (Maybe event))
        (IORef Int)
        (IORef (Map.IntMap (IORef (event -> Narrative '[Event] (Narrative self super) ()))))
    deriving Eq

data Behavior self super event
  = BehaviorToken
      (IORef [(Int,Signal self super event)])
      (IORef (event -> Narrative '[Event] (Narrative self super) ()))
  deriving Eq

{-# INLINE construct #-}
construct :: (Monad super', MonadIO super')
          => Maybe event -> super' (Signal self super event)
construct mevent = liftIO $ do
  current   <- newIORef mevent
  count     <- newIORef 0
  behaviors <- newIORef Map.empty
  return $ Signal current count behaviors

{-# INLINE constructSelf #-}
constructSelf :: (Monad super, MonadIO super)
              => Maybe event -> Narrative self super (Signal self super event)
constructSelf = construct

-- Slightly more specific than necessary to avoid required type signatures.
{-# INLINE runner #-}
runner :: (Monad super, MonadIO super, Monad super')
       => super (Signal self super' (Narrative self super' ()))
runner = do
  current   <- newIORef mevent
  count     <- newIORef 0
  behaviors <- newIORef $ Map.fromList [(-1,super)]
  return $ Signal current count behaviors

{-# INLINE current #-}
current :: (Monad super, MonadIO super)
        => Signal self super e -> Narrative self super (Maybe e)
current (Signal cur _ _) = liftIO $ readIORef cur

{-# INLINE behavior #-}
behavior :: (Monad super', MonadIO super')
         => Signal self super event
         -> (event -> Narrative '[Event] (Narrative self super) ())
         -> super' (BehaviorToken self super event)
behavior sig@(Signal _ count behaviors) newBehavior = liftIO $ do
  c <- atomicModifyIORef' count $ \c ->
         let c' = c + 1
         in c' `seq` (c',c)
  s <- newIORef [(c,sig)]
  b <- newIORef newBehavior
  atomicModifyIORef' behaviors $ \bs ->
    let bs' = Map.insert (c,b) bs
    in bs' `seq` (bs',())
  return (Behavior s b)

{-# INLINE mergeS #-}
mergeS :: (Monad super', MonadIO super')
       => Signal self super event
       -> Signal self super event
       -> super' ( Signal self super event
                 , Behavior self super event
                 , Behavior self super event
                 )
mergeS sig0 sig1 = do
  sig <- construct Nothing
  bt0 <- behavior sig0 $ \_ -> signal sig
  bt1 <- behavior sig1 $ \_ -> signal sig
  return (sig,bt0,bt1)

{-# INLINE zipS #-}
zipS :: ( Monad super, MonadIO super
        , Monad super', MonadIO super'
        )
     => Signal self super event
     -> Signal self super event'
     -> super' ( Signal self super (Maybe event,Maybe event')
               , Behavior self super event
               , Behavior self super event'
               )
zipS sig0@(Signal cur0 cnt0 bs0) sig1@(Signal cur1 cnt1 bs1) = do
  sig <- construct Nothing
  bt0 <- behavior sig0 $ \_ e0 -> do
    mc1 <- current sig1
    signal sig (Just e0,mc1)
  bt1 <- behavior sig1 $ \_ e1 -> do
    mc0 <- current sig0
    signal sig (mc0,Just e1)
  return (sig,bt0,bt1)

{-# INLINE mapS #-}
mapS :: ( Monad super, MonadIO super
        , Monad super', MonadIO super'
        )
     => Signal self super event
     -> (event -> event')
     -> super' ( Signal self super event'
               , Behavior self super event'
               )
mapS sig f = do
  sig' <- construct Nothing
  bt   <- behavior sig $ \_ -> signal sig' . f
  return (sig',bt)

{-# INLINE filterS #-}
filterS :: ( Monad super, MonadIO super
           , Monad super', MonadIO super'
           )
        => Signal self super event
        -> (event -> Maybe event')
        -> super' ( Signal self super event'
                  , Behavior self super event'
                  )
filterS sig f = do
  sig' <- construct Nothing
  bt   <- behavior sig $ \_ e -> forM_ (f e) (signal sig')
  return (sig',bt)

{-# INLINE duplate #-}
duplicate :: (Monad super, MonadIO super)
          => Behavior self super event
          -> Signal self super event
          -> super' ()
duplicate (Behavior s b) (Signal _ _ bs_) = liftIO $ do
  c <- atomicModifyIORef' count $ \c ->
         let c' = c + 1
         in c' `seq` (c',c)
  atomicModifyIORef' bs_ $ \bs ->
    let bs' = Map.insert c b bs
    in bs' `seq` (bs',())

{-# INLINE removeFrom #-}
removeFrom :: (Monad super', MonadIO super')
           => Behavior self super event
           -> Signal self super event
           -> super' ()
removeFrom (Behavior s b) from@(Signal _ _ bs_) = liftIO $ do
  forM_ s $ \(c,sig) ->
    when (sig == from) $
      atomicModifyIORef' bs_ $ \bs ->
        let bs' = Map.delete c bs
        in bs' `seq` (bs',())

{-# INLINE clear #-}
clear :: (Monad super', MonadIO super')
      => Signal self super e -> super' ()
clear (Signal _ _ bs) = liftIO $ writeIORef bs Map.empty

{-# INLINE stop #-}
stop :: (Monad super', MonadIO super')
     => Behavior self super a -> super' ()
stop (Behavior s b) =
  liftIO $ forM_ s unbind'
  where
    unbind' (c,Signal _ _ bs_) = atomicModifyIORf' bs_ $ \bs ->
      let bs' = Map.delete c bs
      in bs' `seq` (bs',())


{-# INLINE signal #-}
signal :: (Monad super, MonadIO super)
       => Signal self super event
       -> event
       -> Narrative self super ()
signal sig e = do
  let Signal _ _ bs_ = sig
  bs <- liftIO $ readIORef bs_
  seeded <- mapM (Map.toList bs) $ \(c,f_) ->
    f <- liftIO $ readIORef f_
    return (bs_,c,f e)
  go seeded
  where
    go [] = return ()
    go ((bs_,c,f):bs) = do
      f <- liftIO $ readIORef f_
      del <- start bs_ c f
      go bs
      where
        start bs_ c f = do
          del <- go' f
          when del $
            liftIO $ atomicModifyIORef' bs_ $ \bs ->
              let bs' = Map.delete c bs
              in bs' `seq` (bs',())
          where
            go' (Return _)  = return False
            go' (Fail e)    = Fail e -- One behavior can clobber an entire process.
            go' (Super sup) = Super (fmap go' sup)
            go' (Say msg k) =
              case prj msg of
                Nothing -> Say msg (go' . k)
                Just x ->
                  case x of
                    Become f' x -> do
                      liftIO $ writeIORef f_ f'
                      go' (k x)
                    Continue -> return False
                    End      -> return True
                    Subsignal sig' e' x -> do
                      let Signal _ _ bs'_ = sig'
                      bs' <- liftIO $ readIORef bs'_
                      seeded <- mapM (Map.toList bs') $ \(c',f'_) ->
                        f' <- liftIO $ readIORef f'_
                        return (bs'_,c',f' e')
                      go ((bs_,c,k x):bs ++ seeded)

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
