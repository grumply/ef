{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Context.Set.Lexemes
    ( Set(..)
    , become
    ) where



import Ef.Core.Narrative
import Ef.Core.Object
import Ef.Core.Inflect

import Ef.Context.Set.Lexicon



become
    :: Inflections contexts lexicon
    => Object contexts environment
    -> Say Set lexicon environment ()

become newObj =
    say (Set newObj ())
