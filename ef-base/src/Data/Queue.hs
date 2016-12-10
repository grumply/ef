{-# language CPP #-}
module Data.Queue
  ( Queue
  , newQueue
  , arrive
  , collect
  ) where

import Ef

#ifndef __GHCJS__
import Control.Concurrent.Chan.Unagi
#else
import Control.Concurrent.STM
#endif
import Data.Monoid

#ifndef __GHCJS__
data Queue a = Queue {-# UNPACK #-} !(InChan a) {-# UNPACK #-} !(OutChan a)
#else
data Queue a = Queue {-# UNPACK #-} !(TQueue a)
#endif
  deriving Eq

newQueue :: (Monad super, MonadIO super)
         => super (Queue a)
#ifndef __GHCJS__
newQueue = do
  (i,o) <- liftIO newChan
  return (Queue i o)
#else
newQueue = liftIO $ Queue <$> newTQueueIO
#endif

arrive :: (Monad super, MonadIO super)
       => Queue a -> a -> super ()
#ifndef __GHCJS__
arrive (Queue i o) a = liftIO $ writeChan i a
#else
arrive (Queue queue) a = liftIO $ atomically $ writeTQueue queue a
#endif

collect :: (Monad super, MonadIO super)
        => Queue a -> super a
#ifndef __GHCJS__
collect (Queue i o) = liftIO $ readChan o
#else
collect (Queue queue) = liftIO $ atomically $ readTQueue queue
#endif
