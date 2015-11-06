{-# LANGUAGE FlexibleContexts #-}
module Data.Promise
  ( Promise, newPromiseIO, newPromise
  , demand, demandIO
  , fulfill, fulfillIO
  ) where

import Mop.Core
import Mop.IO
import Effect.Exception

import Control.Concurrent

-- expected to be used in Actors, Agents and Fork.

newtype Promise a = Promise { getPromise :: MVar a }

newPromiseIO :: IO (Promise a)
newPromiseIO = Promise <$> newEmptyMVar

newPromise :: (MIO m,Has Throw fs m) => Plan fs m (Promise a)
newPromise = io newPromiseIO

demand :: (MIO m,Has Throw fs m) => Promise a -> Plan fs m a
demand = io . demandIO

demandIO :: Promise a -> IO a
demandIO = readMVar . getPromise

fulfill :: (MIO m,Has Throw fs m) => Promise a -> a -> Plan fs m Bool
fulfill = (io .) . fulfillIO

fulfillIO :: Promise a -> a -> IO Bool
fulfillIO (Promise p) a = tryPutMVar p a
