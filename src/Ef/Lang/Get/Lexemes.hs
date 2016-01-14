{-# LANGUAGE FlexibleContexts #-}
module Ef.Lang.Get.Lexemes
    ( Lexicon(..)
    , introspect
    ) where



import Ef.Core.Narrative
import Ef.Core.Object

import Ef.Lang.Get.Lexicon



introspect
    :: Say Lexicon lexicon environment (Object contexts environment)

introspect =
    do
        -- note the need to Reset; this should help the GC avoid holding
        -- onto the Object since it is only ever used once.
        say (Reify ())
        slf <- say (Get id)
        say (Reset ())
        return slf

