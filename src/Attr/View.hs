{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Attr.View
    ( Viewing, ask, asks, local
    , Viewable, reader
    ) where

import Mop.Core

data Viewing r k = Viewing (r -> k)

data Viewable r k = Viewable r k

instance Symmetry (Viewable r) (Viewing r) where
    symmetry use (Viewable r k) (Viewing rk) = symmetry use (r,k) rk

{-# INLINE ask #-}
ask :: Is (Viewing r) fs m => Plan fs m r
ask = self (Viewing id)

{-# INLINE asks #-}
asks :: Is (Viewing r) fs m => (r -> a) -> Plan fs m a
asks f = self (Viewing f)

{-# INLINE reader #-}
reader :: Uses (Viewable r) fs m => r -> Attribute (Viewable r) fs m
reader r = Viewable r pure

{-# INLINE local #-}
local :: forall fs m r. Is (Viewing r) fs m => (r -> r) -> Plan fs m r -> Plan fs m r
local f p0 = go p0 where
    go p = case p of
        Step sym bp -> case prj sym of
            Just (Viewing (r :: r -> b)) -> Step (inj (Viewing (r . f)))
                                             (\b -> go (bp b))
            Nothing -> Step sym (\b -> go (bp b))
        M m -> M (fmap go m)
        Pure r -> Pure r
