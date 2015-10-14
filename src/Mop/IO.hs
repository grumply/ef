{-# LANGUAGE InstanceSigs #-}
module Mop.IO where

import Mop

-- this class and module exists to avoid a core dependency on mtl/transformers.

class Monad m => MIO m where
  mio :: IO a -> PlanT fs m a

instance MIO IO where
  mio :: IO a -> PlanT fs IO a
  mio = lift
