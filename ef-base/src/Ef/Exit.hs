module Ef.Exit (enter) where

import Ef
import Ef.Sync

exiting :: (Monad c, ms <: '[Sync]) => ((a' -> Ef ms c a) -> Ef ms c r) -> Synchronized a' a b' b ms c r
exiting computation = synchronized $ \up _ -> computation up

-- | Scope a short-circuit continuation.
--
-- @
--     enter $ \exit -> do
--         ...
--         when _ (exit r)
--         ...
-- @
enter :: (Monad c, ms <: '[Sync]) => ((r -> Ef ms c b) -> Ef ms c r) -> Ef ms c r
enter computation = runSync (return +>> exiting computation)
