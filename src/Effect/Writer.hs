{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Writer
    ( Trace, tell
    , Writer, tracer, writer, written
    ) where

import Mop.Core

import Data.Monoid

data Trace r k = Trace r k

data Writer r k = Writer r (r -> k)

instance Pair (Writer r) (Trace r) where
    pair p (Writer _ rk) (Trace r k) = pair p rk (r,k)

{-# INLINE tell #-}
tell :: (Has (Trace w) fs m) => w -> Plan fs m ()
tell w = self (Trace w ())

{-# INLINE writer #-}
writer :: (Monoid w,Uses (Writer w) fs m) => Attribute (Writer w) fs m
writer = tracer mempty (<>)

{-# INLINE tracer #-}
tracer :: Uses (Writer w) fs m => w -> (w -> w -> w) -> Attribute (Writer w) fs m
tracer w0 f = Writer w0 $ \w' fs ->
    let Writer w k = view fs
    in pure $ fs .= Writer (f w w') k

{-# INLINE written #-}
written :: Uses (Writer w) fs m => Object fs m -> w
written fs = let Writer w _ = view fs in w
