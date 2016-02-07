{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Writer.Messages
    ( Writer(..)
    , tell
    ) where


import Ef.Narrative


data Writer r k = Tell r k

tell :: w -> Invoke (Writer w) self super ()
tell w = self (Tell w ())


{-# INLINE tell #-}
