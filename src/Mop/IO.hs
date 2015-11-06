{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Mop.IO where

import Mop.Core
import Effect.Exception
import qualified Control.Exception as Exc

unsafe  :: Lift IO m => IO a -> Plan fs m a
unsafe   = lift

masked_ :: (Has Throw fs m,Lift IO m) => IO a -> Plan fs m a
masked_  = lift . Exc.mask_

masked  :: (Has Throw fs m,Lift IO m) => ((forall (a :: *). IO a -> IO a) -> IO b) -> Plan fs m b
masked   = lift . Exc.mask

io      :: (Has Throw fs m,Lift IO m) => IO a -> Plan fs m a
io ioa   = do
  ea <- lift (Exc.try ioa)
  case ea of
    Left e -> throw (e :: SomeException)
    Right r -> return r
