{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Mop.IO where

import Mop.Core
import Effect.Exception
import qualified Control.Exception as Exc

class Monad m => MIO m where
    unsafe  :: Has Throw fs m => IO a -> Plan fs m a
    masked_ :: Has Throw fs m => IO a -> Plan fs m a
    masked  :: Has Throw fs m => ((forall (a :: *). IO a -> IO a) -> IO b) -> Plan fs m b
    io      :: Has Throw fs m => IO a -> Plan fs m a

instance MIO IO where
    unsafe   = lift
    masked_  = lift . Exc.mask_
    masked   = lift . Exc.mask
    io ioa   = do
      ea <- lift (Exc.try ioa)
      case ea of
        Left e -> throw (e :: SomeException)
        Right r -> return r

instance (MIO m,Has Throw fs m) => MIO (Plan fs m) where
    unsafe   = lift . unsafe
    masked_  = lift . masked_
    masked   = lift . masked
    io       = lift . io
