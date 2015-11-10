{-# LANGUAGE FlexibleContexts #-}
module Data.Promise
  ( Promise, newPromiseIO, newPromise
  , demand, demandIO
  , fulfill, fulfillIO
  ) where

import Mop.Core
import Lang.Global.IO
import Lang.Global.Except

import Control.Concurrent

-- expected to be used in Actors, Agents and Fork.

newtype Promise a = Promise { getPromise :: MVar a }

newPromiseIO :: IO (Promise a)
newPromiseIO = Promise <$> newEmptyMVar

newPromise :: (Lift IO m,Is Excepting fs m) => Pattern fs m (Promise a)
newPromise = io newPromiseIO

demand :: (Lift IO m,Is Excepting fs m) => Promise a -> Pattern fs m a
demand = io . demandIO

demandIO :: Promise a -> IO a
demandIO = readMVar . getPromise

fulfill :: (Lift IO m,Is Excepting fs m) => Promise a -> a -> Pattern fs m Bool
fulfill = (io .) . fulfillIO

fulfillIO :: Promise a -> a -> IO Bool
fulfillIO (Promise p) a = tryPutMVar p a
