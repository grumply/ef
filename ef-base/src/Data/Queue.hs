{-# language ViewPatterns #-}
module Data.Queue
  ( Queue
  , newQueue
  , arrive
  , arriveMany
  , collect
  ) where

import Ef

import Control.Concurrent

import Data.Foldable as F

data Queue a = Queue (MVar ()) (MVar [a])

newQueue :: (Monad super, MonadIO super)
         => super (Queue a)
newQueue = liftIO $ Queue <$> newMVar () <*> newEmptyMVar

arrive :: (Monad super, MonadIO super)
       => Queue a -> a -> super ()
arrive (Queue barrier q) a = liftIO $
  withMVar barrier $ \_ -> do
    mq_ <- tryTakeMVar q
    case mq_ of
        Just q_ -> putMVar q (a:q_)
        Nothing -> putMVar q [a]

arriveMany :: (Monad super, MonadIO super, Foldable f)
           => Queue a -> f a -> super ()
arriveMany (Queue barrier q) (F.toList -> as) = liftIO $
  withMVar barrier $ \_ -> do
    mq_ <- tryTakeMVar q
    case mq_ of
        Just q_ -> putMVar q ((reverse as) ++ q_)
        Nothing -> putMVar q (reverse as)

collect :: (Monad super, MonadIO super)
        => Queue a -> super [a]
collect (Queue barrier q) = liftIO $
  withMVar barrier $ \_ -> do
    q_ <- takeMVar q
    return $ reverse q_
