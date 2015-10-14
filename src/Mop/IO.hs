module Mop.IO where

import Mop
import Effect.Exception
import qualified Control.Exception as Exc
import System.IO.Unsafe

{-# INLINE inlineUnsafePerformMIO #-}
inlineUnsafePerformMIO :: IO a -> PlanT fs m a
inlineUnsafePerformMIO = Pure . unsafePerformIO

{-# NOINLINE unsafePerformMIO #-}
unsafePerformMIO :: IO a -> PlanT fs m a
unsafePerformMIO = Pure . unsafePerformIO

class Monad m => MIO m where
  unsafeMIO :: IO a -> PlanT fs m a
  mio :: Has Throw fs m => IO a -> PlanT fs m a

instance MIO IO where
  {-# INLINE unsafeMIO #-}
  unsafeMIO = lift
  {-# INLINE mio #-}
  mio ioa = do
    ea <- lift $ Exc.try ioa
    case ea of
      Left e -> throw (e :: SomeException)
      Right r -> return r
