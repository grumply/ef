{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Context.Note.Lexemes
    ( Note(..)
    , note
    ) where


import Ef.Core.Narrative

import Ef.Context.Note.Lexicon



note
    :: w
    -> Say (Note w) lexicon environment ()

note w =
    say (Note w ())



{-# INLINE note #-}
