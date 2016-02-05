{- | A wrapper around MVar in a promise-y style. -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AutoDeriveTypeable #-}
module Data.Promise
    ( Promise
    , newPromiseIO
    , newPromise

    , demand
    , demandIO

    , fulfill
    , fulfillIO
    ) where


import Ef
import Ef.IO

import Control.Concurrent
import GHC.Generics


-- | Promise represents a variable that:
--   1. may be set once
--   2. may be checked for a value
--   3. may be read many times
--   4. may be blocked on for a read
--   5. may be shared across threads
newtype Promise result = Promise { getPromise :: MVar result }
    deriving Eq


-- | Construct a new empty promise in IO.
newPromiseIO :: IO (Promise result)
newPromiseIO = Promise <$> newEmptyMVar


-- | Construct a new empty promise.
newPromise :: (Monad super, Lift IO super)
           => Narrative messages super (Promise result)
newPromise = io newPromiseIO


-- | Demand a promised value, blocking until it is fulfilled.
demand :: (Monad super, Lift IO super)
       => Promise result -> Narrative messages super result
demand = io . demandIO


-- | Demand a promised value in IO, blocking until it is fulfilled.
demandIO :: Promise result -> IO result
demandIO = readMVar . getPromise


-- | Fulfill a promise. Returns a Bool where False
-- denotes that the `Promise` has already been fulfilled.
fulfill :: (Monad super, Lift IO super)
        => Promise result -> result -> Narrative messages super Bool
fulfill = (io .) . fulfillIO


-- | Fulfill a promise in IO. Returns a Bool where False
-- denotes that the `Promise` has already been fulfilled.
fulfillIO :: Promise result -> result -> IO Bool
fulfillIO (Promise p) result = tryPutMVar p result


-- | Poll a `Promise` for the result of a `fulfill`. Does not block.
fulfilled :: (Monad super, Lift IO super)
          => Promise result -> Narrative messages super Bool
fulfilled = io . fulfilledIO


-- | Poll a `Promise` for the result of a `fulfill` in IO. Does not block.
fulfilledIO :: Promise result -> IO Bool
fulfilledIO (Promise p) = isEmptyMVar p
