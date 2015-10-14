module Mop.IO where

import Mop
import Effect.Exception
import qualified Control.Exception as Exc

-- this class and module exists to avoid a core dependency on mtl/transformers.

class Monad m => MIO m where
  mio :: Has Throw fs m => IO a -> PlanT fs m a

instance MIO IO where
  mio ioa = do
    ea <- lift $ Exc.try ioa
    case ea of
      Left e -> throw (e :: SomeException)
      Right r -> return r
