{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.Get.Messages
    ( Get(..)
    , introspect
    ) where



import Ef.Narrative
import Ef.Object


data Get k where
    Reset :: k -> Get k
    Reify :: k -> Get k
    Get :: (Object gs m -> k)  -> Get k


introspect :: Invoke Get self super (Object methods super)
introspect = do
    -- note the need to Reset; this should help the GC avoid holding
    -- onto the Object since it is only ever used once.
    self (Reify ())
    slf <- self (Get id)
    self (Reset ())
    return slf

