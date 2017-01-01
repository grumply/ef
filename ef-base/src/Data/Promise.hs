{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# language RecordWildCards #-}
module Data.Promise
    ( Promise
    , Process
    , processId
    , Status(..)
    , ProcessListener
    , PromiseListener
    , SomeProcessListener

    , process
    , promise

    , await
    , awaitMaybe
    , demand
    , demandMaybe

    , fake
    , forlorn

    , complete
    , fulfill
    , completed
    , fulfilled
    , onComplete
    , onFulfill

    , abort
    , forsake
    , aborted
    , forsook
    , onAbort
    , onForsake

    , notify
    , status
    , onNotify
    , onNotify'

    , completed
    , resolved
    ) where


import Ef

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.IORef
import Data.Unique

data Status status result
  = Notify status
  | Aborted SomeException
  | Complete result
  deriving (Eq)

instance Eq SomeException where
  (==) e1 e2 = (show e1) == (show e2)

eitherToStatus :: Either SomeException result -> Status status result
eitherToStatus (Left se) = Aborted se
eitherToStatus (Right r) = Complete r

-- | Promise represents a variable that:
--
-- Note:
--   If the result of the promise is never demanded and the promise is never
--   fulfilled or forsaken, the listeners will not be notified. TODO: refine
--   the API to avoid this problem by constructing the promise with the function
--   whose result or aborture will resolve it?
data Process status result = Process
  { processId :: Unique
  , processStatus :: IORef (Maybe status)
  , processListeners :: MVar (Int,[(Int,Status status result -> IO ())])
  , processResult :: MVar (Either SomeException result)
  } deriving (Eq)

type Promise result = Process () result

-- | Construct a `fulfill`ed `Promise` with the given value.
fake :: (Monad super, MonadIO super)
     => a -> super (Promise a)
fake a = do
  p <- promise
  fulfill p a
  return p

-- | Construct a `forsook` `Promise` with the given `SomException`.
forlorn :: (Monad super, MonadIO super)
        => SomeException -> super (Promise a)
forlorn se = do
  p <- promise
  forsake p se
  return p

-- | Construct a new `Process`.
process :: (Monad super, MonadIO super)
        => super (Process status result)
process = liftIO $
  Process
  <$> newUnique
  <*> newIORef Nothing
  <*> newMVar (1,[])
  <*> newEmptyMVar

-- | Construct a new `Promise`.
promise :: (Monad super, MonadIO super)
        => super (Promise result)
promise = process

-- | Await the result of a `Process`, blocking until it is completed or aborted.
-- Lifts BlockedIndefinitelyOnMVar into the result if the procoess is never
-- to be completed or aborted.
await :: (Monad super, MonadIO super)
      => Process status result -> super (Either SomeException result)
await Process {..} = do
  (ls,res) <- liftIO $
                catch (do { res <- readMVar processResult
                          ; return (return (),res)
                          }
                      ) $ \biom@BlockedIndefinitelyOnMVar -> do
        -- is this race-correct? I think it is implicitly since
        -- the existence of the BIOMVar implies a lack of threads with a
        -- fulfill/forsake call.
        modifyMVar processListeners $ \(n,pls) -> do
          let pls' = reverse $ map snd pls
              e = toException biom
              r = Aborted e
          return ((1,[]),(forM_ pls' ($ r),Left e))
  liftIO ls
  return res

-- | Await the return of a `Process`, blocking until it is completed or aborted.
-- Casts the result to a Maybe by discarding the possible exception. Lifts
-- BlockedIndefinitelyOnMVar into the Nothing case if the process is never
-- to be completed or aborted.
awaitMaybe :: (Monad super, MonadIO super)
            => Process status result -> super (Maybe result)
awaitMaybe p = do
  esr <- await p
  return $ either (const Nothing) Just esr

-- | Demand a `Promise`d value, blocking until it is fulfilled or forsaken.
-- Lifts BlockedIndefinitelyOnMVar into the result if the promise is never
-- to be fulfilled or forsaken. `demand` is simply a specialization of await
-- from `Process` to `Promise`
demand :: (Monad super, MonadIO super)
       => Promise result -> super (Either SomeException result)
demand = await

-- | Demand a `Promise`d value, blocking until it is fulfilled or forsaken.
-- Casts the result to a Maybe by discarding the possible exception. Lifts
-- BlockedIndefinitelyOnMVar into the Nothing case if the promise is never
-- to be fulfilled or forsaken.
demandMaybe :: (Monad super, MonadIO super)
            => Promise result -> super (Maybe result)
demandMaybe = awaitMaybe

-- | Complete a `Process`. Returns a Bool where False denotes the `Process`
-- has already been completed.
complete :: (Monad super, MonadIO super)
         => Process status result -> result -> super Bool
complete (Process _ _ ls_ p) res = liftIO $ do
  (ls,r) <- modifyMVar ls_ $ \(_,pls) -> do
    let r = Complete res
    didPut <- tryPutMVar p (Right res)
    let ls = if didPut then
                let pls' = reverse $ map snd pls
                in forM_ pls' ($ r)
              else
                return ()
    return ((1,[]),(ls,didPut))
  ls
  return r

-- | Resolve a `Promise`. Returns a Bool where False denotes the `Promise`
-- has already been resolved.
fulfill :: (Monad super, MonadIO super)
        => Promise result -> result -> super Bool
fulfill = complete

-- | Poll a `Process` for a `complete`d status. Does not block but instead
-- returns False if the `Process` has not been `complete`d. Returns False if the
-- `Process` has aborted.
completed :: (Monad super, MonadIO super)
          => Process status result -> super Bool
completed (Process _ _ _ p) = do
  mres <- liftIO (tryTakeMVar p)
  case mres of
    Just (Right _) -> return True
    _ -> return False

-- | Poll a `Promise` for `fulfill`ed status. Does not block but instead returns
-- False if the `Promise` has not been `fulfill`ed. Returns False if the `Promise`
-- was forsaken.
fulfilled :: (Monad super, MonadIO super)
          => Promise result -> super Bool
fulfilled = completed

notify :: (Monad super, MonadIO super)
       => Process status result -> status -> super ()
notify (Process _ st_ ls_ p) s =
  liftIO $ do
    writeIORef st_ (Just s)
    (_,pls) <- readMVar ls_
    let r = Notify s
    sequence_ .
      reverse {- eww -} .
        map (($ r) . snd) $
          pls

-- | Abort a `Process`. Returns a Bool where False denotes the `Process` has
-- already been resolved to either completed or aborted.
abort :: (Monad super, MonadIO super, Exception e)
     => Process status result -> e -> super Bool
abort (Process _ _ ls_ p) e =
  liftIO $ do
    (ls,r) <- modifyMVar ls_ $ \(_,pls) -> do
      let ex = toException e
          r = Aborted ex
      didPut <- tryPutMVar p (Left ex)
      let ls = if didPut then
                let pls' = reverse $ map snd pls
                in forM_ pls' ($ r)
              else
                return ()
      return ((1,[]),(ls,didPut))
    ls
    return r

-- | Forsake a `Promise`. Returns a Bool where False denotes the `Promise`
-- has already been resolved.
forsake :: (Monad super, MonadIO super, Exception e)
        => Promise result -> e -> super Bool
forsake = abort

-- | Poll a `Process` for `abort`ed status. Does not block but instead returns False
-- if the `Process` has not completed or aborted. Returns False if the `Process` was
-- `complete`d.
aborted :: (Monad super, MonadIO super)
        => Process status result -> super Bool
aborted (Process _ _ _ p) = do
  mres <- liftIO (tryTakeMVar p)
  case mres of
    Just (Left _) -> return True
    _ -> return False

-- | Poll a `Promise` for the result of a `forsake`. Does not block but instead
-- returns False if the `Promise` has not been resolved. Returns False if the
-- `Promise` was `fulfill`ed.
forsook :: (Monad super, MonadIO super)
        => Promise result -> super Bool
forsook = aborted

status :: (Monad super, MonadIO super)
       => Process status result -> super (Maybe status)
status (Process _ st_ _ _) = liftIO (readIORef st_)

-- | Poll a `Process` for a completion status. Does not block but instead returns
-- False if the `Process` is unfinished.
finished :: (Monad super, MonadIO super)
         => Process status result -> super Bool
finished (Process _ _ _ p) = not <$> liftIO (isEmptyMVar p)

-- | Poll a `Promise` for a resolution status. Does not block but instead returns
-- False if the `Promise` is unrsolved.
resolved :: (Monad super, MonadIO super)
         => Promise result -> super Bool
resolved = finished

data ProcessListener status result =
  ProcessListener (Process status result) Int

type PromiseListener result = ProcessListener () result

data SomeProcessListener =
  forall status result. SomeProcessListener (ProcessListener status result)

onComplete :: (Monad super, MonadIO super)
           => Process status result
           -> (result -> IO ())
           -> super (ProcessListener status result)
onComplete pr@(Process _ _ ls_ p) f =
  liftIO $ do
    (ls,r) <- modifyMVar ls_ $ \(n,pls) -> do
      mres <- tryTakeMVar p
      let f' res =
            case res of
              Complete r -> f r
              _ -> return ()
      case mres of
        Nothing -> do
          let l = (n,f')
              !n' = succ n
          return ((n',l:pls),(return (),ProcessListener pr n))
        Just r ->
          return ((0,[]),(f' $ eitherToStatus r,ProcessListener pr 0))
    ls
    return r

onFulfill :: (Monad super, MonadIO super)
          => Promise result
          -> (result -> IO ())
          -> super (PromiseListener result)
onFulfill = onComplete

onAbort :: (Monad super, MonadIO super)
       => Process status result
       -> (SomeException -> IO ())
       -> super (ProcessListener status result)
onAbort pr@(Process _ _ ls_ p) f =
  liftIO $ do
    (ls,r) <- modifyMVar ls_ $ \(n,pls) -> do
      mres <- tryTakeMVar p
      let f' res =
            case res of
              Aborted se -> f se
              _ -> return ()
      case mres of
        Nothing -> do
          let l = (n,f')
              !n' = succ n
          return ((n',l:pls),(return (),ProcessListener pr n))
        Just r ->
          return ((0,[]),(f' $ eitherToStatus r,ProcessListener pr 0))
    ls
    return r

onForsake :: (Monad super, MonadIO super)
          => Promise result
          -> (SomeException -> IO ())
          -> super (PromiseListener result)
onForsake = onAbort

onNotify :: (Monad super, MonadIO super)
         => Process status result
         -> (status -> IO ())
         -> super (ProcessListener status result)
onNotify pr@(Process _ _ ls_ p) f =
  liftIO $ do
    (ls,r) <- modifyMVar ls_ $ \(n,pls) -> do
      mres <- tryTakeMVar p
      let f' res =
            case res of
              Notify st -> f st
              _ -> return ()
      case mres of
        Nothing -> do
          let l = (n,f')
              !n' = succ n
          return ((n',l:pls),(return (),ProcessListener pr n))
        Just r ->
          return ((0,[]),(return (),ProcessListener pr 0))
    ls
    return r

onNotify' :: (Monad super, MonadIO super)
          => Process status result
          -> (status -> IO ())
          -> super (ProcessListener status result)
onNotify' pr@(Process _ st_ ls_ p) f =
  liftIO $ do
    (ls,r) <- modifyMVar ls_ $ \(n,pls) -> do
      mres <- tryTakeMVar p
      let f' res =
            case res of
              Notify st -> f st
              _ -> return ()
      mst <- readIORef st_
      case mres of
        Nothing -> do
          let l = (n,f')
              !n' = succ n
          return $
            case mst of
              Nothing ->
                ((n',l:pls),(return (),ProcessListener pr n))
              Just st ->
                ((n',l:pls),(f st,ProcessListener pr n))
        Just r ->
          return $
            case mst of
              Just st ->
                ((0,[]),(f st,ProcessListener pr 0))
              _ ->
                ((0,[]),(return (),ProcessListener pr 0))
    ls
    return r

cancelListener :: (Monad super, MonadIO super)
               => ProcessListener status result -> super ()
cancelListener (ProcessListener (Process _ _ ls_ _) i) =
  liftIO $ modifyMVar_ ls_ $ \(n,pls) -> do
    let !pls' = filter ((/=) i . fst) pls
    return (n,pls')
