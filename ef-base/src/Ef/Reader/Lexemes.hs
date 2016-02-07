{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Context.View.Lexemes
    ( View(..)
    , ask
    , asks
    , local
    ) where



import Ef.Core.Narrative
import Ef.Context.View.Lexicon



ask
    :: Say (View r) scope parent r

ask =
    say (View id)



asks
    :: (r -> a)
    -> Say (View r) scope parent a

asks f =
    say (View f)



local
    :: forall scope parent r.
       Knows (View r) scope parent
    => (r -> r)
    -> Narrative scope parent r
    -> Narrative scope parent r

local f =
    go
  where

    go (Fail e) =
        Fail e

    go (Say sym bp) =
        case prj sym of

            Just (View (r :: r -> b)) ->
                let
                    newSymbol =
                        inj (View (r . f))

                in
                    Say newSymbol (go . bp)

            Nothing ->
                Say sym (\b -> go (bp b))

    go (Super m) =
        Super (fmap go m)

    go (Return r) =
        Return r



{-# INLINE ask #-}
{-# INLINE asks #-}
{-# INLINE local #-}
