{- | A wrapper around MVar in a promise-y style. -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AutoDeriveTypeable #-}
module Data.Promise
    (
      -- * Promise
      Promise
    , newPromise
    , fake

      -- * Ef Use
    , fulfill
    , fulfilled
    , demand

      -- * IO Creation and Use
    , newPromiseIO
    , fakeIO
    , fulfillIO
    , fulfilledIO
    , demandIO
    ) where


import Ef

import Control.Concurrent


-- | Promise represents a variable that:
--
--   (1) may be set only once with `fulfill` but does not block
--   2. may be polled with `fulfilled` for a value
--   3. may be blocked on with `demand` for a value
--   4. may be read many times with `demand`
--   5. may be shared across threads
newtype Promise result = Promise { getPromise :: MVar result }
    deriving (Eq)

fake :: (Monad super, MonadIO super)
     => a -> Narrative self super (Promise a)
fake a = do
  p <- newPromise
  fulfill p a
  return p

fakeIO :: a -> IO (Promise a)
fakeIO a = do
  p <- newPromiseIO
  fulfillIO p a
  return p

-- | Construct a new un`fulfill`ed `Promise`.
newPromise :: (Monad super, MonadIO super)
           => Narrative self super (Promise result)
newPromise = liftIO newPromiseIO


-- | Demand a `Promise`d value, blocking until it is fulfilled. Lifts
-- `BlockedIndefinitelyOnMVar` into the `Narrative` if the underlying
-- `demandIO` throws it when a `Promise` can never be `fulfill`ed.
demand :: (Monad super, MonadIO super)
       => Promise result -> Narrative self super result
demand = liftIO . demandIO


-- | Fulfill a `Promise`. Returns a Bool where False
-- denotes that the `Promise` has already been fulfilled.
fulfill :: (Monad super, MonadIO super)
        => Promise result -> result -> Narrative self super Bool
fulfill = (liftIO .) . fulfillIO


-- | Poll a `Promise` for the result of a `fulfill`. Does not block but instead
-- returns False if the `Promise` has already been `fulfill`ed.
fulfilled :: (Monad super, MonadIO super)
          => Promise result -> Narrative self super Bool
fulfilled = liftIO . fulfilledIO


-- | Construct a new un`fulfill`ed `Promise` in IO.
newPromiseIO :: IO (Promise result)
newPromiseIO = Promise <$> newEmptyMVar


-- | Demand a `Promise`d value in IO, blocking until it is fulfilled.
demandIO :: Promise result -> IO result
demandIO = readMVar . getPromise


-- | Fulfill a `Promise` in IO. Returns a Bool where False
-- denotes that the `Promise` has already been fulfilled.
fulfillIO :: Promise result -> result -> IO Bool
fulfillIO (Promise p) = tryPutMVar p


-- | Poll a `Promise` for the result of a `fulfill` in IO. Does not block.
fulfilledIO :: Promise result -> IO Bool
fulfilledIO (Promise p) = isEmptyMVar p
