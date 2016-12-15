module Ef.Exit (enter) where

import Ef
import Ef.Sync

exiting :: (Monad c, '[Sync] <: ms) => ((a' -> Code ms c a) -> Code ms c result) -> Synchronized a' a b' b ms c result
exiting computation = synchronized $ \up _ -> computation up

-- | Scope a short-circuit continuation.
--
-- @
--     enter $ \exit -> do
--         ...
--         when _ (exit result)
--         ...
-- @
enter :: (Monad c, '[Sync] <: ms) => ((result -> Code ms c b) -> Code ms c result) -> Code ms c result
enter computation = runSync (return +>> exiting computation)
