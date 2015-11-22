{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Lang.Scoped.Generate
  ( Generator(..)
  , generate
  , each
  , discard
  , every
  ) where



import Ef.Core
import Ef.Lang.Scoped.Weave


import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F

import Unsafe.Coerce



newtype Generator fs m a =
    Select
        { enumerate
              :: Is Weaving fs m
              => Producer a fs m ()
        }



instance Functor (Generator fs m)
  where

    fmap f (Select p) =
        let
          produce =
              producer . (flip id . f)

        in
          Select (p //> produce)



instance Applicative (Generator fs m)
  where

    pure a = Select (producer ($ a))



    mf <*> mx =
        let
          produce f x =
              let
                yields yield =
                    yield (f x)

              in
                producer yields

        in
          Select
              $ for (enumerate mf)
              $ for (enumerate mx)
              . produce



instance Monad (Generator fs m)
  where

    return a =
        let
          yields yield =
              yield a

        in
          Select (producer yields)



    m >>= f =
        Select $ for (enumerate m) (enumerate . f)



    fail _ =
        mzero



instance Alternative (Generator fs m)
  where

    empty =
        let
          ignore = const (return ())

        in
          Select (producer ignore)



    p1 <|> p2 =
        Select $ woven $ \up dn ->
            let
              run xs = runWoven (enumerate xs) (unsafeCoerce up) (unsafeCoerce dn)

            in
              do
                run p1
                run p2



instance MonadPlus (Generator fs m)
  where

    mzero =
        empty



    mplus =
        (<|>)



instance Monoid (Generator fs m a)
  where

    mempty =
        empty



    mappend =
        (<|>)



generate
    :: Is Weaving fs m
    => Generator fs m a
    -> Pattern fs m ()
generate l =
    let
      complete =
          do
            _ <- l
            mzero

    in
      weave (enumerate complete)



each
    :: ( Is Weaving fs m
       , F.Foldable f
       )
    => f a
    -> Producer' a fs m ()
each xs =
    let
      def =
          return ()

      yields yield =
          F.foldr (const . yield) def xs

    in
      producer yields



discard
    :: Monad m
    => t
    -> Woven fs a' a b' b m ()
discard _ =
    let
      ignore _ _ =
          return ()

    in
      Woven ignore



every
    :: Is Weaving fs m
    => Generator fs m a
    -> Producer' a fs m ()
every it =
    discard >\\ enumerate it



-- | Inlines

{-# INLINE generate #-}
{-# INLINE each #-}
{-# INLINE discard #-}
{-# INLINE every #-}
