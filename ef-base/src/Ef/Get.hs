module Ef.Get (Get, get, introspect) where

import Ef
import Data.Bifunctor
import Unsafe.Coerce

data Get k where
    Get
      :: { reflection :: (Object ts c,k)
         , resetter :: k
         , reifier :: k
         } -> Get k


    Reset_
      :: { reset :: k } -> Get k

    Reify_
      :: { reify :: k } -> Get k

    View_
      :: { view :: (Object gs m -> k) } -> Get k

instance Functor Get where
  fmap f Get {..} = Get (fmap f reflection) (f resetter) (f reifier)
  fmap f Reset_ {..} = Reset_ (f reset)
  fmap f Reify_ {..} = Reify_ (f reify)
  fmap f View_ {..} = View_ (fmap f view)

pattern Reset = Reset_ (Return ())
pattern Reify = Reify_ (Return ())
pattern View f = View_ f

instance Delta Get Get where
  delta eval Get {..} Reify_ {..} = eval reifier reify
  delta eval Get {..} Reset_ {..} = eval resetter reset
  delta eval Get {..} View_ {..} =
    let (o,viewer) = reflection
        self = unsafeCoerce o
    in eval viewer (view self)

get :: forall ts c. (Monad c, ts <. '[Get]) => Get (Action ts c)
get = Get (undefined,reflector) resetter reifier
  where
    reflector o =
      let Module (g :: Get (Action ts c)) o = o
      in case g of
           Get {..} -> pure $ Module Get { reflection = (o,reflector), .. } o

    resetter o =
      let Module (g :: Get (Action ts c)) o = o
      in case g of
           Get {..} -> pure $ Module Get { reflection = first (const undefined) reflection, .. } o

    reifier = pure
{-# INLINE get #-}

introspect :: (Monad c, ms <: '[Get], ts <. '[Get], Delta (Modules ts) (Messages ms))
           => Ef ms c (Object ts c)
introspect = do
    -- does the reset help the GC or is it unnecessary?
    Send Reify
    slf <- Send (View Return)
    Send Reset
    return slf
{-# INLINE introspect #-}
