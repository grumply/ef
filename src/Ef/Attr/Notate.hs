{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Attr.Notate
    ( Noting, note
    , Notatable, notator, writer, noted
    ) where

import Ef.Core

import Data.Monoid

-- | Symbol

data Noting r k = Noting r k

-- | Symbol Construct

{-# INLINE note #-}
note :: (Is (Noting w) fs m) => w -> Pattern fs m ()
note w = self (Noting w ())

-- | Attribute

data Notatable r k = Notatable r (r -> k)

-- | Attribute Construct

{-# INLINE notator #-}
notator :: Uses (Notatable w) fs m => w -> (w -> w -> w) -> Attribute (Notatable w) fs m
notator w0 f = Notatable w0 $ \w' fs ->
    let Notatable w k = view fs
    in pure $ fs .= Notatable (f w w') k

{-# INLINE writer #-}
writer :: (Monoid w,Uses (Notatable w) fs m) => Attribute (Notatable w) fs m
writer = notator mempty (<>)

-- | Attribute/Symbol Symmetry

instance Witnessing (Notatable r) (Noting r) where
    witness use (Notatable _ rk) (Noting r k) = witness use rk (r,k)

-- | Extended API

{-# INLINE noted #-}
noted :: Uses (Notatable w) fs m => Object fs m -> w
noted fs = let Notatable w _ = view fs in w
