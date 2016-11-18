module Data.Queue
  ( Queue
  , newQueue
  , arrive
  , collect
  ) where

import Ef

import Control.Concurrent.STM
import Data.Monoid

data Queue a = Queue {-# UNPACK #-} !(TQueue a)
  deriving Eq

newQueue :: (Monad super, MonadIO super)
         => super (Queue a)
newQueue = liftIO $ Queue <$> newTQueueIO

arrive :: (Monad super, MonadIO super)
       => Queue a -> a -> super ()
arrive (Queue queue) a = liftIO $ atomically $ writeTQueue queue a

collect :: (Monad super, MonadIO super)
        => Queue a -> super a
collect (Queue queue) = liftIO $ atomically $ readTQueue queue
