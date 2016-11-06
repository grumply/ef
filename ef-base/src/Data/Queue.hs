module Data.Queue
  ( Queue
  , newQueue
  , newQueueFrom
  , arrive
  , collect
  ) where

-- If anything more than this is needed, switch to TQueue.

import Ef

import Control.Concurrent.STM
import Data.Monoid

data Queue a = Queue {-# UNPACK #-} !(TMVar [a])
  deriving Eq

newQueue :: (Monad super, MonadIO super)
         => super (Queue a)
newQueue = liftIO $ Queue <$> newEmptyTMVarIO

newQueueFrom :: (Monad super, MonadIO super)
             => [a] -> super (Queue a)
newQueueFrom q = liftIO $ Queue <$> newTMVarIO q

arrive :: (Monad super, MonadIO super)
       => Queue a -> a -> super ()
arrive (Queue queue) a = liftIO $ atomically $ do
  mq <- tryTakeTMVar queue
  case mq of
    Nothing -> putTMVar queue [a]
    Just q  -> putTMVar queue (a:q)

collect :: (Monad super, MonadIO super)
        => Queue a -> super [a]
collect (Queue queue) = liftIO $
  reverse <$> atomically (takeTMVar queue)
