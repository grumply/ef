{-# LANGUAGE InstanceSigs #-}
module Mop.IO where

import Mop

-- this class and module exists to avoid a core dependency on mtl/transformers.

class MIO m where
  liftMIO :: IO a -> Plan fs m a

instance MIO IO where
  liftMIO :: IO a -> Plan fs IO a
  liftMIO = lift

-- Extract this out to mop-transformers/mop-mtl:
-- instance MonadIO m => MIO m where
--   liftMIO :: m a -> Plan fs m a
--   liftMIO = liftIO
