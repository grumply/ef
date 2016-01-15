{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Set.Lexemes
    ( Set(..)
    , become
    ) where



import Ef.Core.Narrative
import Ef.Core.Object
import Ef.Core.Inflect

import Ef.Lang.Set.Lexicon



become
    :: Inflections contexts lexicon
    => Object contexts environment
    -> Say Set lexicon environment ()

become newObj =
    say (Set newObj ())
