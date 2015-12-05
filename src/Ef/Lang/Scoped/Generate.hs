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



newtype Generator scope parent a =
    Select
        { enumerate
              :: Is Weaving scope parent
              => Producer a scope parent ()
        }



instance Functor (Generator scope parent)
  where

    fmap f (Select p) =
        let
          produce =
              producer . (flip id . f)

        in
          Select (p //> produce)



instance Applicative (Generator scope parent)
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



instance Monad (Generator scope parent)
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



instance Alternative (Generator scope parent)
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



instance MonadPlus (Generator scope parent)
  where

    mzero =
        empty



    mplus =
        (<|>)



instance Monoid (Generator scope parent a)
  where

    mempty =
        empty



    mappend =
        (<|>)



generate
    :: Is Weaving scope parent
    => Generator scope parent a
    -> Pattern scope parent ()
generate l =
    let
      complete =
          do
            _ <- l
            mzero

    in
      weave (enumerate complete)



each
    :: ( Is Weaving scope parent
       , F.Foldable f
       )
    => f a
    -> Producer' a scope parent ()
each xs =
    let
      def =
          return ()

      yields yield =
          F.foldr (const . yield) def xs

    in
      producer yields



discard
    :: Monad parent
    => t
    -> Woven scope a' a b' b parent ()
discard _ =
    let
      ignore _ _ =
          return ()

    in
      Woven ignore



every
    :: Is Weaving scope parent
    => Generator scope parent a
    -> Producer' a scope parent ()
every it =
    discard >\\ enumerate it



-- | Inlines

{-# INLINE generate #-}
{-# INLINE each #-}
{-# INLINE discard #-}
{-# INLINE every #-}
