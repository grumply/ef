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


instance ( Uses (Viewable r) attrs parent
         , Binary r
         )
    => Binary (Attribute (Viewable r) attrs parent)
  where

    get =
        do
          r <- get
          return (reader r)

    put (Viewable r _) =
        put r


ask
    :: Is (Viewing r) scope parent
    => Pattern scope parent r

ask =
    self (Viewing id)



asks
    :: Is (Viewing r) scope parent
    => (r -> a)
    -> Pattern scope parent a

asks f =
    self (Viewing f)



reader
    :: Uses (Viewable r) scope parent
    => r
    -> Attribute (Viewable r) scope parent

reader r =
    Viewable r pure



local
    :: forall scope parent r.
       Is (Viewing r) scope parent
    => (r -> r)
    -> Pattern scope parent r
    -> Pattern scope parent r

local f =
    go
  where

    go (Fail e) =
        Fail e

    go (Send sym bp) =
        case prj sym of

            Just (Viewing (r :: r -> b)) ->
                let
                  newSymbol =
                    inj (Viewing (r . f))

                in
                  Send newSymbol (go . bp)

            Nothing -> Send sym (\b -> go (bp b))

    go (Super m) =
        Super (fmap go m)

    go (Pure r) =
        Pure r



{-# INLINE ask #-}
{-# INLINE asks #-}
{-# INLINE reader #-}
{-# INLINE local #-}
