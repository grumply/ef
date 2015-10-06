module Effect.Exception
  (throw,catch,throws,Throw(..),ThrowHandler(..),Exception(..),SomeException(..))
  where

import Mop
import Control.Exception hiding (throw,catch)

data Throw        k = Throw         SomeException    k
data ThrowHandler k = ThrowHandler (SomeException -> k)

-- catch :: (Has Throw symbols m, Exception e)
--       => (e -> Plan symbols m a) -> Plan symbols m a -> Plan symbols m a
catch sub = removeStep $ \stp@(Step syms bp) ->
  maybe stp (\(Throw se _) -> maybe stp sub (fromException se)) (prj syms)

-- throw :: (Has Throw symbols, Exception e) => e -> Plan symbols m a
throw e = symbol (Throw (toException e) undefined)

-- throws :: ThrowHandler k
throws = ThrowHandler (\se -> error $ "Uncaught exception: " ++ show se)

instance Pair ThrowHandler Throw where
  pair p (ThrowHandler k) (Throw e k') = p (k e) k'
