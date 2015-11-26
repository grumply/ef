{- | A wrapper around MVar in a promise-y style. -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AutoDeriveTypeable #-}
module Ef.Data.Promise
    ( Promise
    , newPromiseIO
    , newPromise

    , demand
    , demandIO

    , fulfill
    , fulfillIO
    ) where



import Ef.Core
import Ef.Lang.IO

import Control.Concurrent
import GHC.Generics



newtype Promise a =

    Promise
        { getPromise
              :: MVar a
        }

  deriving Eq



newPromiseIO
    :: IO (Promise a)

newPromiseIO =
    Promise <$> newEmptyMVar



newPromise
    :: ( Monad m
       , Lift IO m
       )
    => Pattern fs m (Promise a)

newPromise =
    io newPromiseIO



demand
    :: ( Monad m
       , Lift IO m
       )
    => Promise a
    -> Pattern fs m a

demand =
    io . demandIO



demandIO
    :: Promise a
    -> IO a

demandIO =
    readMVar . getPromise



fulfill
    :: ( Monad m
       , Lift IO m
       )
    => Promise a
    -> a
    -> Pattern fs m Bool

fulfill =
    (io .) . fulfillIO



fulfillIO
    :: Promise a
    -> a
    -> IO Bool

fulfillIO (Promise p) a =
    tryPutMVar p a
