module Ef.Lang.Exit
    ( enter
    ) where


import Ef.Narrative
import Ef.Knot


exiting
    :: (Can Knot self, Monad super)
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
    :: (Can Knot self, Monad super)
    => (    (result -> Narrative self super b)
         -> Narrative self super result
       )
    -> Narrative self super result
enter computation = linearize (return +>> exiting computation)
