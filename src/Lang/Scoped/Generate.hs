{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Lang.Scoped.Generate
  ( Generator(..), generate, each, discard, every
  ) where

import Mop.Core
import Lang.Scoped.Weave

import Control.Applicative
import Control.Monad

import qualified Data.Foldable as F

import Unsafe.Coerce

newtype Generator fs m a = Select { enumerate :: Is Weaving fs m => Producer a fs m () }

instance Functor (Generator fs m) where
    fmap f (Select p) = Select (p //> (\x -> producer $ \yield -> yield (f x)))

instance Applicative (Generator fs m) where
    pure a = Select (producer $ \yield -> yield a)
    mf <*> mx =
        Select (for (enumerate mf)
                    (\f -> for (enumerate mx)
                               (\x -> (producer $ \yield -> yield (f x)))
                    )
               )

instance Monad (Generator fs m) where
    return a = Select (producer $ \yield -> yield a)
    m >>= f = Select (for (enumerate m) (\a -> enumerate (f a)))
    fail _ = mzero

instance Alternative (Generator fs m) where
    empty = Select (producer $ \_ -> return ())
    p1 <|> p2 =
        Select $ woven $ \up dn -> do
            runWoven (enumerate p1) (unsafeCoerce up) (unsafeCoerce dn)
            runWoven (enumerate p2) (unsafeCoerce up) (unsafeCoerce dn)

instance MonadPlus (Generator fs m) where
    mzero = empty
    mplus = (<|>)

instance Monoid (Generator fs m a) where
    mempty = empty
    mappend = (<|>)

{-# INLINE generate #-}
generate :: Is Weaving fs m => Generator fs m a -> Pattern fs m ()
generate l = weave (enumerate (l >> mzero))

{-# INLINE each #-}
each :: Is Weaving fs m => F.Foldable f => f a -> Producer' a fs m ()
each xs = producer $ \yield -> F.foldr (\a p -> yield a >> p) (return ()) xs

{-# INLINE discard #-}
discard :: Monad m => t -> Woven fs a' a b' b m ()
discard _ = Woven $ \_ _ -> return ()

{-# INLINE every #-}
every :: Is Weaving fs m => Generator fs m a -> Producer' a fs m ()
every it = discard >\\ enumerate it
