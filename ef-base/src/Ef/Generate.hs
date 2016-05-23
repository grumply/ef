module Ef.Generate
  ( Generator(..)
  , generate
  , each
  , discard
  , every
  ) where

import Ef.Narrative
import Ef.Knot

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F

import Unsafe.Coerce

newtype Generator self super a =
    Select
        { enumerate
              :: ('[Knot] <: self, Monad super)
              => Producer a self super ()
        }

instance Functor (Generator self super)
  where

    fmap f (Select p) =
        Select (p //> (producer . flip id . f))

instance Applicative (Generator self super)
  where

    pure a = Select (producer ($ a))

    mf <*> mx =
        let produce f x = producer ($ f x)
        in Select
              $ for (enumerate mf)
              $ for (enumerate mx)
              . produce

instance Monad (Generator self super)
  where

    return a =
        let yields yield = yield a
        in Select (producer yields)

    m >>= f = Select $ for (enumerate m) (enumerate . f)

    fail _ = mzero

instance Alternative (Generator self super)
  where

    empty =
        let ignore = const (return ())
        in Select (producer ignore)

    p1 <|> p2 =
        Select $ knotted $ \up dn ->
            let run xs = runKnotted (enumerate xs) (unsafeCoerce up) (unsafeCoerce dn)
            in do run p1
                  run p2

instance MonadPlus (Generator self super)
  where

    mzero = empty

    mplus = (<|>)

instance Monoid (Generator self super a)
  where

    mempty = empty

    mappend = (<|>)

generate :: ('[Knot] <: self, Monad super)
         => Generator self super a -> Narrative self super ()
generate l = runKnot (enumerate (l >> mzero))

each :: ('[Knot] <: self, Monad super, F.Foldable f)
     => f a -> Producer' a self super ()
each xs =
    let yields yield = F.foldr (const . yield) (return ()) xs
    in producer yields

discard :: Monad super => t -> Knotted self a' a b' b super ()
discard _ =
    let ignore _ _ = return ()
    in Knotted ignore

every :: ('[Knot] <: self, Monad super)
      => Generator self super a -> Producer' a self super ()
every it =
    discard >\\ enumerate it

{-# INLINE generate #-}
{-# INLINE each #-}
{-# INLINE discard #-}
{-# INLINE every #-}
