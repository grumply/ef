module Effect.Exception
  ( throw, catch, Throw
  , throws, Throws
  , Exception,SomeException(..)
  ) where

import Mop
import Control.Exception (SomeException(..),Exception(..))

data Throw  k = Throw         SomeException    k
data Throws k = Throws (SomeException -> k)

-- catch :: (Has Throw symbols m, Exception e)
--       => (e -> Plan symbols m a) -> Plan symbols m a -> Plan symbols m a
catch sub = removeStep $ \stp@(Step syms bp) ->
  maybe stp (\(Throw se _) -> maybe stp sub (fromException se)) (prj syms)

-- throw :: (Has Throw symbols, Exception e) => e -> Plan symbols m a
throw e = symbol (Throw (toException e) undefined)

-- throws :: Throws k
throws = Throws (\se -> error $ "Uncaught exception: " ++ show se)

instance Pair Throws Throw where
  pair p (Throws k) (Throw e k') = p (k e) k'
