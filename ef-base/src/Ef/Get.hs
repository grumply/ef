module Ef.Get (Get, get, introspect) where

import Ef

import Unsafe.Coerce

data Get k where
    Get :: (Object methods super,k) -> k -> k -> Get k
    Reset :: k -> Get k
    Reify :: k -> Get k
    View :: (Object gs m -> k)  -> Get k

instance Ma Get Get where
    ma use (Get _ _ k) (Reify k')    = use k k'
    ma use (Get (o,k) _ _) (View ok) = use k (ok (unsafeCoerce o))
    ma use (Get _ k _) (Reset k')    = use k k'

get :: (Monad super, '[Get] <. traits)
    => Trait Get traits super
get = Get (undefined,reifier) resetter pure
  where

    resetter fs =
        case view fs of

            Get (_,reifies) reset gets ->
                pure $ fs .= Get (undefined,reifies) reset gets

    reifier fs =
        case view fs of

            Get _ reset gets ->
                pure $ fs .= Get (fs,reifier) reset gets
{-# INLINE get #-}

introspect :: (Monad super, '[Get] <: self)
           => Narrative self super (Object methods super)
introspect = do
    -- does the reset help the GC or is it unnecessary?
    self (Reify ())
    slf <- self (View id)
    self (Reset ())
    return slf
{-# INLINE introspect #-}
