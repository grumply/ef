module Ef.Exit
    ( enter
    ) where

import Ef.Narrative
import Ef.Sync

exiting
    :: (Monad super, '[Sync] <: self)
    => ((a' -> Narrative self super a) -> Narrative self super result)
    -> Synchronized a' a b' b self super result
exiting computation = synchronized $ \up _ -> computation up

-- | Scope a short-circuit continuation.
--
-- @
--     enter $ \exit -> do
--         ...
--         when _ (exit result)
--         ...
-- @
enter
    :: (Monad super, '[Sync] <: self)
    => (    (result -> Narrative self super b)
         -> Narrative self super result
       )
    -> Narrative self super result
enter computation = runSync (return +>> exiting computation)
