{-# LANGUAGE KindSignatures, ImpredicativeTypes #-} -- keep an eye on this
module Mop.IO where

import Mop
import Effect.Exception
import qualified Control.Exception as Exc
import System.IO.Unsafe

{-# INLINE inlineUnsafePerformMIO #-}
inlineUnsafePerformMIO :: forall fs m a. Monad m => IO a -> Plan fs m a
inlineUnsafePerformMIO = (return :: forall z. z -> Plan fs m z) . unsafePerformIO

{-# NOINLINE unsafePerformMIO #-}
unsafePerformMIO :: forall fs m a. Monad m => IO a -> Plan fs m a
unsafePerformMIO = (return :: forall z. z -> Plan fs m z) . unsafePerformIO

class Monad m => MIO m where
  unsafeMIO  :: Has Throw fs m => IO a -> Plan fs m a
  maskedMIO_ :: Has Throw fs m => IO a -> Plan fs m a
  maskedMIO  :: Has Throw fs m => ((forall (a :: *). IO a -> IO a) -> IO b) -> Plan fs m b
  mio        :: Has Throw fs m => IO a -> Plan fs m a

instance MIO IO where
  {-# INLINE unsafeMIO #-}
  unsafeMIO = lift
  {-# INLINE maskedMIO_ #-}
  maskedMIO_ = lift . Exc.mask_
  {-# INLINE maskedMIO #-}
  maskedMIO = lift . Exc.mask
  {-# INLINE mio #-}
  mio ioa = do
    ea <- lift $ Exc.try ioa
    case ea of
      Left e -> throw (e :: SomeException)
      Right r -> return r
