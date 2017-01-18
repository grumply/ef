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
import Control.Concurrent.Chan
#endif
import Data.Monoid

#ifndef __GHCJS__
data Queue a = Queue {-# UNPACK #-} !(InChan a) {-# UNPACK #-} !(OutChan a)
#else
data Queue a = Queue {-# UNPACK #-} !(Chan a)
#endif
  deriving Eq

newQueue :: (MonadIO super)
         => super (Queue a)
#ifndef __GHCJS__
newQueue = do
  (i,o) <- liftIO newChan
  return (Queue i o)
#else
newQueue = liftIO $ Queue <$> newChan
#endif

arrive :: (MonadIO super)
       => Queue a -> a -> super ()
#ifndef __GHCJS__
arrive (Queue i o) a = liftIO $ writeChan i a
#else
arrive (Queue ch) a = liftIO $ writeChan ch a
#endif

collect :: (MonadIO super) => Queue a -> super a
#ifndef __GHCJS__
collect (Queue i o) = liftIO $ readChan o
#else
collect (Queue ch) = liftIO $ readChan ch
#endif
