{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Scoped.Exit.Lexemes
    ( Exit(..)
    , Exits(..)
    , exits
    ) where



import Ef.Core.Narrative
import Ef.Lang.Scoped.Exit.Lexicon

import Unsafe.Coerce


data Exits result scope parent =
    Exits
        {
          exit
              :: forall b.
                 result
              -> Narrative scope parent b
        }



exits
    :: Knows Exit scope parent
    => (    Exits result scope parent
         -> Narrative scope parent result
       )
    -> Narrative scope parent result

exits f =
    do
      scope <- say (FreshScope id)
      rewrite scope $ f
          Exits
              { exit =
                    \a ->
                        say (Done scope a)
              }
  where

    rewrite rewriteScope =
        go
      where

        go (Fail e) =
            Fail e

        go (Return r) =
            Return r

        go (Super m) =
            Super (fmap go m)

        go (Say lex bp) =
            let
              ignore =
                  Say lex (go . bp)

            in
              case prj lex of

                  Just x ->
                      case x of

                          Done currentScope a

                              | currentScope == rewriteScope ->
                                    Return (unsafeCoerce a)

                              | otherwise ->
                                    ignore

                          _ ->
                              ignore

                  _ ->
                      ignore



{-# INLINE exits #-}
