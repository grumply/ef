module Ef.Exit (enter) where

import Ef
import Ef.Sync

exiting :: (Monad c, '[Sync] <: ms) => ((a' -> Code ms c a) -> Code ms c r) -> Synchronized a' a b' b ms c r
exiting computation = synchronized $ \up _ -> computation up

-- | Scope a short-circuit continuation.
--
-- @
--     enter $ \exit -> do
--         ...
--         when _ (exit r)
--         ...
-- @
enter :: (Monad c, '[Sync] <: ms) => ((r -> Code ms c b) -> Code ms c r) -> Code ms c r
enter computation = runSync (return +>> exiting computation)
