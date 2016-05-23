module Ef.Lang.Exit
    ( enter
    ) where

import Ef.Narrative
import Ef.Knot

exiting
    :: (Monad super, '[Knot] <: self)
    => ((a' -> Narrative self super a) -> Narrative self super result)
    -> Knotted a' a b' b self super result
exiting computation = knotted $ \up _ -> computation up

-- | Scope a short-circuit continuation.
--
-- @
--     enter $ \exit -> do
--         ...
--         when _ (exit result)
--         ...
-- @
enter
    :: (Monad super, '[Knot] <: self)
    => (    (result -> Narrative self super b)
         -> Narrative self super result
       )
    -> Narrative self super result
enter computation = linearize (return +>> exiting computation)
