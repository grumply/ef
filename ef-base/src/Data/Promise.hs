{- | A wrapper around MVar in a promise-y style. -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# language RecordWildCards #-}
module Data.Promise
    ( Promise
    , PromiseListener
    , SomePromiseListener

    , newPromise

    , demand
    , demandMaybe

    , fake
    , forlorn

    , fulfill
    , fulfilled
    , onFulfill

    , forsake
    , forsook
    , onForsake

    , resolved
    ) where


import Ef

import Control.Concurrent
import Control.Exception hiding (catch)
import Control.Monad


-- | Promise represents a variable that:
--
--   (1) may be set only once with `fulfill` but does not block
--   2. may be polled with `fulfilled` for a value
--   3. may be blocked on with `demand` for a value
--   4. may be read many times with `demand`
--   5. may be shared across threads
--
-- Note:
--   If the result of the promise is never demanded and the promise is never
--   fulfilled or forsaken, the listeners will not be notified. TODO: refine
--   the API to avoid this problem by constructing the promise with the function
--   whose result or failure will resolve it.
data Promise result = Promise
  { promiseListeners :: MVar (Int,[(Int,Either SomeException result -> IO ())])
  , promiseResult :: MVar (Either SomeException result)
  } deriving (Eq)

-- | Construct a `fulfill`ed `Promise` with the given value.
fake :: (Monad super, MonadIO super)
     => a -> super (Promise a)
fake a = do
  p <- newPromise
  fulfill p a
  return p

-- | Construct a `forsook` `Promise` with the given `SomException`.
forlorn :: (Monad super, MonadIO super)
        => SomeException -> super (Promise a)
forlorn se = do
  p <- newPromise
  forsake p se
  return p

-- | Construct a new un`fulfill`ed `Promise`.
newPromise :: (Monad super, MonadIO super)
           => super (Promise result)
newPromise = liftIO $ Promise <$> newMVar (1,[]) <*> newEmptyMVar

-- | Demand a `Promise`d value, blocking until it is fulfilled or forsaken.
-- Lifts BlockedIndefinitelyOnMVar into the result if the promise is never
-- to be fulfilled or forsaken.
demand :: (Monad super, MonadIO super, MonadCatch super)
       => Promise result -> super (Either SomeException result)
demand Promise {..} = do
  (ls,res) <- catch (do { res <- liftIO $ readMVar $ promiseResult; return (return (),res) })
    $ \biom@BlockedIndefinitelyOnMVar -> do
        -- is this race-correct? I think it is implicitly race-correct since
        -- the existence of the BIOMVar implies a lack of threads with a
        -- fulfill/forsake call.
        liftIO $ modifyMVar promiseListeners $ \(n,pls) -> do
          let pls' = reverse $ map snd pls
              r = Left $ toException biom
          return ((1,[]),(forM_ pls' ($ r),Left $ toException biom))
  liftIO ls
  return res

-- | Demand a `Promise`d value, blocking until it is fulfilled or forsaken.
-- Casts the result to a Maybe by discarding the possible exception. Lifts
-- BlockedIndefinitelyOnMVar into the Nothing case if the promise is never
-- to be fulfilled or forsaken.
demandMaybe :: (Monad super, MonadIO super, MonadCatch super)
            => Promise result -> super (Maybe result)
demandMaybe p = do
  esr <- demand p
  return $ either (const Nothing) Just esr

-- | Fulfill a `Promise`. Returns a Bool where False denotes the `Promise`
-- has already been resolved.
fulfill :: (Monad super, MonadIO super)
        => Promise result -> result -> super Bool
fulfill (Promise ls_ p) res = do
  liftIO $ do
    (ls,r) <- modifyMVar ls_ $ \(_,pls) -> do
      let r = Right res
      didPut <- tryPutMVar p r
      let ls = if didPut then
                 let pls' = reverse $ map snd pls
                 in forM_ pls' ($ r)
               else
                 return ()
      return ((1,[]),(ls,didPut))
    ls
    return r

-- | Poll a `Promise` for the result of a `fulfill`. Does not block but instead
-- returns False if the `Promise` has not been `fulfill`ed. Returns False if the
-- `Promise` was forsaken.
fulfilled :: (Monad super, MonadIO super)
          => Promise result -> super Bool
fulfilled (Promise _ p) = do
  mres <- liftIO (tryTakeMVar p)
  case mres of
    Just (Right _) -> return True
    _ -> return False

-- | Forsake a `Promise`. Returns a Bool where False denotes the `Promise`
-- has already been resolved.
forsake :: (Monad super, MonadIO super, Exception e)
        => Promise result -> e -> super Bool
forsake (Promise ls_ p) e =
  liftIO $ do
    (ls,r) <- modifyMVar ls_ $ \(_,pls) -> do
      let r = Left $ toException e
      didPut <- tryPutMVar p r
      let ls = if didPut then
                let pls' = reverse $ map snd pls
                in forM_ pls' ($ r)
              else
                return ()
      return ((1,[]),(ls,didPut))
    ls
    return r

-- | Poll a `Promise` for the result of a `forsake`. Does not block but instead
-- returns False if the `Promise` has not been `fulfill`ed. Returns False if the
-- `Promise` was fulfilled.
forsook :: (Monad super, MonadIO super)
        => Promise result -> super Bool
forsook (Promise _ p) = do
  mres <- liftIO (tryTakeMVar p)
  case mres of
    Just (Left _) -> return True
    _ -> return False

-- | Poll a `Promise` for the result of a either a `forsake` or `fulfill`.
-- Does not block but instead returns False if the `Promise` has not been
-- `fulfill`ed.
resolved :: (Monad super, MonadIO super)
         => Promise result -> super Bool
resolved (Promise _ p) = not <$> liftIO (isEmptyMVar p)

data PromiseListener result =
  PromiseListener (Promise result) Int

data SomePromiseListener =
  forall result. SomePromiseListener (PromiseListener result)

onFulfill :: (Monad super, MonadIO super)
          => Promise result
          -> (result -> IO ())
          -> super (PromiseListener result)
onFulfill pr@(Promise ls_ p) f =
  liftIO $ do
    (ls,r) <- modifyMVar ls_ $ \(n,pls) -> do
      mres <- tryTakeMVar p
      case mres of
        Nothing -> do
          let f' = either (const (return ())) f
              l = (n,f')
              !n' = succ n
          return ((n',l:pls),(return (),PromiseListener pr n))
        Just r ->
          return ((0,[]),(either (const (return ())) f r,PromiseListener pr 0))
    ls
    return r

onForsake :: (Monad super, MonadIO super)
          => Promise result
          -> (SomeException -> IO ())
          -> super (PromiseListener result)
onForsake pr@(Promise ls_ p) f =
  liftIO $ do
    (ls,r) <- modifyMVar ls_ $ \(n,pls) -> do
      mres <- tryTakeMVar p
      case mres of
        Nothing -> do
          let f' = either f (const (return ()))
              l = (n,f')
              !n' = succ n
          return ((n',l:pls),(return (),PromiseListener pr n))
        Just r ->
          return ((0,[]),(either f (const (return ())) r,PromiseListener pr 0))
    ls
    return r

cancelPromiseListener :: (Monad super, MonadIO super)
                      => PromiseListener result -> super ()
cancelPromiseListener (PromiseListener (Promise ls_ _) i) =
  liftIO $ modifyMVar_ ls_ $ \(n,pls) -> do
    let !pls' = filter ((/=) i . fst) pls
    return (n,pls')
