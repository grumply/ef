{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.Exit
    ( Exiting
    , exits
    , Exit(..)

    , Exitable
    , exiter
    ) where



import Ef.Core

import Data.Binary
import Unsafe.Coerce



data Exiting k
  where

    FreshScope
        :: (Int -> k)
        -> Exiting k

    Done
        :: Int
        -> a
        -> Exiting k



data Exitable k
  where

    Exitable
        :: Int
        -> k
        -> Exitable k



instance Uses Exitable attrs parent
    => Binary (Attribute Exitable attrs parent)
  where

    get =
        return exiter



    put _ =
        pure ()



exiter
    :: Uses Exitable attrs parent
    => Attribute Exitable attrs parent

exiter =
    Exitable 0 $ \fs ->
        let
          Exitable i k =
              view fs

          i' =
              succ i

        in
          i' `seq` pure $ fs .=
              Exitable i' k



instance Witnessing Exitable Exiting
  where

    witness use (Exitable i k) (FreshScope ik) =
        use k (ik i)



data Exit result scope parent =
    Exit
        {
          exit
              :: forall b.
                 result
              -> Pattern scope parent b
        }



exits
    :: Is Exiting scope parent
    => (    Exit result scope parent
         -> Pattern scope parent result
       )
    -> Pattern scope parent result

exits f =
    do
      scope <- self (FreshScope id)
      rewrite scope $ f
          Exit
              { exit =
                    \a ->
                        self (Done scope a)
              }
  where

    rewrite rewriteScope =
        go
      where

        go (Fail e) =
            Fail e

        go (Pure r) =
            Pure r

        go (Super m) =
            Super (fmap go m)

        go (Send sym bp) =
            let
              ignore =
                  Send sym (go . bp)

            in
              case prj sym of

                  Just x ->
                      case x of

                          Done currentScope a

                              | currentScope == rewriteScope ->
                                    Pure (unsafeCoerce a)

                              | otherwise ->
                                    ignore

                          _ ->
                              ignore

                  _ ->
                      ignore



{-# INLINE exiter #-}
{-# INLINE exits #-}
