module Ef.Lang.Fiber
    ( Fiber(..)
    , fiber
    ) where



import Ef.Lang.Knot
import Ef.Core.Narrative



data Action lexicon environment
    where

        Fork
            :: (    (Ops lexicon environment status result -> Narrative lexicon environment result)
                 -> Narrative lexicon environment (Operation status result)
               )
            -> Action lexicon environment

        Focus
            :: Action lexicon environment

        Yield
            :: Narrative lexicon environment ()
            -> Action lexicon environment

        
