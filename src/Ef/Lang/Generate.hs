{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Lang.Generate
  ( Generator(..)
  , generate
  , each
  , discard
  , every
  ) where



import Ef.Core.Narrative
import Ef.Lang.Knot


import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F

import Unsafe.Coerce



newtype Generator lexicon environment a =
    Select
        { enumerate
              :: Knows Knots lexicon environment
              => Producer a lexicon environment ()
        }



instance Functor (Generator lexicon environment)
  where

    fmap f (Select p) =
        let
          produce =
              producer . (flip id . f)

        in
          Select (p //> produce)



instance Applicative (Generator lexicon environment)
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



instance Monad (Generator lexicon environment)
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



instance Alternative (Generator lexicon environment)
  where

    empty =
        let
          ignore = const (return ())

        in
          Select (producer ignore)



    p1 <|> p2 =
        Select $ knotted $ \up dn ->
            let
              run xs = runKnotted (enumerate xs) (unsafeCoerce up) (unsafeCoerce dn)

            in
              do
                run p1
                run p2



instance MonadPlus (Generator lexicon environment)
  where

    mzero =
        empty



    mplus =
        (<|>)



instance Monoid (Generator lexicon environment a)
  where

    mempty =
        empty



    mappend =
        (<|>)



generate
    :: Knows Knots lexicon environment
    => Generator lexicon environment a
    -> Narrative lexicon environment ()
generate l =
    let
      complete =
          do
            _ <- l
            mzero

    in
      linearize (enumerate complete)



each
    :: ( Knows Knots lexicon environment
       , F.Foldable f
       )
    => f a
    -> Producer' a lexicon environment ()
each xs =
    let
      def =
          return ()

      yields yield =
          F.foldr (const . yield) def xs

    in
      producer yields



discard
    :: Monad environment
    => t
    -> Knotted lexicon a' a b' b environment ()
discard _ =
    let
      ignore _ _ =
          return ()

    in
      Knotted ignore



every
    :: Knows Knots lexicon environment
    => Generator lexicon environment a
    -> Producer' a lexicon environment ()
every it =
    discard >\\ enumerate it



-- | Inlines

{-# INLINE generate #-}
{-# INLINE each #-}
{-# INLINE discard #-}
{-# INLINE every #-}
