{-# LANGUAGE GADTs #-}
module Ef.Lang.Get.Lexicon where

import Ef.Core


data GET k
  where

    Reset
        :: k
        -> Get k

    Reify
        :: k
        -> Get k

    Get
        :: (    Object gs m
             -> k
           )
        -> Get k



introspect
    :: Witnessing (Attrs attrs) (Symbol scope)
    => Method Get scope parent (Object attrs parent)

introspect =
    do
        -- note the need to Reset; this should help the GC avoid a space leak.
        self (Reify ())
        slf <- self (Get id)
        self (Reset ())
        return slf
        
{-# INLINE introspect #-}
