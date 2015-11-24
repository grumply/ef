{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Scoped.Exit
    ( Exiting
    , exits
    , Exit(..)

    , Exitable
    , exiter
    ) where

import Ef.Core
import Unsafe.Coerce



-- | Symbol

data Exiting k
  where

    FreshScope
        :: (Int -> k)
        -> Exiting k

    Done
        :: Int
        -> a
        -> Exiting k



-- | Attribute

data Exitable k
  where

    Exitable
        :: Int
        -> k
        -> Exitable k



-- | Attribute Construct


exiter
    :: Uses Exitable gs m
    => Attribute Exitable gs m

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



-- | Symbol/Attribute Symmetry

instance Witnessing Exitable Exiting
  where

    witness use (Exitable i k) (FreshScope ik) =
        use k (ik i)



-- | Symbol Module

data Exit a fs m =
    Exit
        {
          exit
              :: forall b.
                 a
              -> Pattern fs m b
        }



-- | Local Scoping Construct + Symbol Substitution

exits
    :: Is Exiting fs m
    => (    Exit a fs m
         -> Pattern fs m a
       )
    -> Pattern fs m a

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

        go (M m) =
            M (fmap go m)

        go (Step sym bp) =
            let
              ignore =
                  Step sym (go . bp)

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



-- | Inlines

{-# INLINE exiter #-}
{-# INLINE exits #-}
