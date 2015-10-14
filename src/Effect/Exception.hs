module Effect.Exception
  ( throw, catch, Throw
  , throws, Throws
  , Exception,SomeException(..)
  ) where

import Mop
import Mop.IO
import Control.Exception (SomeException(..),Exception(..))
import qualified Control.Exception as Exc

data Throw  k = Throw         SomeException    k
data Throws k = Throws (SomeException -> k)

-- catch :: (Has Throw symbols m, Exception e)
--       => (e -> PlanT symbols m a) -> PlanT symbols m a -> PlanT symbols m a
catch sub = removeStep $ \stp@(Step syms bp) ->
  maybe stp (\(Throw se _) -> maybe stp sub (fromException se)) (prj syms)

-- throw :: (Has Throw symbols, Exception e) => e -> PlanT symbols m a
throw e = symbol (Throw (toException e) undefined)

-- throws :: Throws k
throws = Throws (\se -> error $ "Uncaught exception: " ++ show se)

instance Pair Throws Throw where
  pair p (Throws k) (Throw e k') = p (k e) k'
