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



newtype Promise result =

    Promise
        { getPromise
              :: MVar result
        }

  deriving Eq



newPromiseIO
    :: IO (Promise result)

newPromiseIO =
    Promise <$> newEmptyMVar



newPromise
    :: ( Monad environment
       , Lift IO environment
       )
    => Narrative lexicon environment (Promise result)

newPromise =
    io newPromiseIO



demand
    :: ( Monad environment
       , Lift IO environment
       )
    => Promise result
    -> Narrative lexicon environment result

demand =
    io . demandIO



demandIO
    :: Promise result
    -> IO result

demandIO =
    readMVar . getPromise



fulfill
    :: ( Monad environment
       , Lift IO environment
       )
    => Promise result
    -> result
    -> Narrative lexicon environment Bool

fulfill =
    (io .) . fulfillIO



fulfillIO
    :: Promise result
    -> result
    -> IO Bool

fulfillIO (Promise p) result =
    tryPutMVar p result
