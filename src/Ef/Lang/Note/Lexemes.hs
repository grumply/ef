{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Note.Lexemes
    ( Lexicon(..)
    , note
    ) where


import Ef.Core.Narrative

import Ef.Lang.Note.Lexicon



note
    :: w
    -> Say (Lexicon w) lexicon environment ()

note w =
    say (Note w ())



{-# INLINE note #-}
