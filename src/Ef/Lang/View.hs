{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.View
    ( Viewing
    , ask
    , asks
    , local

    , Viewable
    , reader
    ) where



import Ef.Core

import Data.Binary



data Viewing r k
  where

    Viewing
        :: (r -> k)
        -> Viewing r k



data Viewable r k
  where

    Viewable
        :: r
        -> k
        -> Viewable r k



instance (Viewable r) `Witnessing` (Viewing r)
  where

    witness use (Viewable r k) (Viewing rk) =
        witness use (r,k) rk


instance ( Uses (Viewable r) gs m
         , Binary r
         )
    => Binary (Attribute (Viewable r) gs m)
  where

    get =
        do
          r <- get
          return (reader r)

    put (Viewable r _) =
        put r


ask
    :: Is (Viewing r) fs m
    => Pattern fs m r

ask =
    self (Viewing id)



asks
    :: Is (Viewing r) fs m
    => (r -> a)
    -> Pattern fs m a

asks f =
    self (Viewing f)



reader
    :: Uses (Viewable r) fs m
    => r
    -> Attribute (Viewable r) fs m

reader r =
    Viewable r pure



local
    :: forall fs m r.
       Is (Viewing r) fs m
    => (r -> r)
    -> Pattern fs m r
    -> Pattern fs m r

local f =
    go
  where

    go (Fail e) =
        Fail e

    go (Step sym bp) =
        case prj sym of

            Just (Viewing (r :: r -> b)) ->
                let
                  newSymbol =
                    inj (Viewing (r . f))

                in
                  Step newSymbol (go . bp)

            Nothing -> Step sym (\b -> go (bp b))

    go (M m) =
        M (fmap go m)

    go (Pure r) =
        Pure r



{-# INLINE ask #-}
{-# INLINE asks #-}
{-# INLINE reader #-}
{-# INLINE local #-}
