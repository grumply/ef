{-# language ViewPatterns #-}
module Data.Queue
  ( Queue(..)
  , newQueueIO, newQueue
  , arriveIO, arrive
  , arriveManyIO, arriveMany
  , collectIO, collect
  ) where

import Ef

import Control.Concurrent

import Data.Foldable as F

data Queue a = Queue (MVar [a])

newQueueIO :: IO (Queue a)
newQueueIO = Queue <$> newEmptyMVar

arriveIO :: Queue a -> a -> IO ()
arriveIO (Queue q) a = do
  mq_ <- tryTakeMVar q
  case mq_ of
      Just q_ -> putMVar q (a:q_)
      Nothing -> putMVar q [a]

arriveManyIO :: Foldable f => Queue a -> f a -> IO ()
arriveManyIO (Queue q) (F.toList -> as) = do
  mq_ <- tryTakeMVar q
  case mq_ of
      Just q_ -> putMVar q ((reverse as) ++ q_)
      Nothing -> putMVar q (reverse as)

collectIO :: Queue a -> IO [a]
collectIO (Queue q) = do
    q_ <- takeMVar q
    return $ reverse q_

newQueue :: (Monad super, MonadIO super)
         => Narrative self super (Queue a)
newQueue = liftIO newQueueIO

arrive :: (Monad super, MonadIO super)
       => Queue a -> a -> Narrative self super ()
arrive q a = liftIO $ arriveIO q a

arriveMany :: (Monad super, MonadIO super, Foldable f)
           => Queue a -> f a -> Narrative self super ()
arriveMany q fs = liftIO $ arriveManyIO q fs

collect :: (Monad super, MonadIO super)
        => Queue a -> Narrative self super [a]
collect q = liftIO $ collectIO q
