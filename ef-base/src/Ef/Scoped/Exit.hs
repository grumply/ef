{-# LANGUAGE FlexibleContexts #-}
module Ef.Lang.Exit
    ( enter
    ) where



import Ef.Core.Narrative
import Ef.Lang.Knot



exiting
    :: Knows Knots lexicon environment
    => ((a' -> Narrative lexicon environment a) -> Narrative lexicon environment result)
    -> Knotted a' a b' b lexicon environment result

exiting computation =
    knotted $ \up _ ->
        computation up



enter
    :: Knows Knots lexicon environment
    => ((result -> Narrative lexicon environment b) -> Narrative lexicon environment result)
    -> Narrative lexicon environment result

enter computation =
    linearize (return +>> exiting computation)
