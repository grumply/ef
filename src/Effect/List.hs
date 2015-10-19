{-# LANGUAGE RankNTypes #-}
module Effect.List where

import Mop
import Mop.Trans
import Effect.Weave

import Control.Applicative
import Control.Monad

import qualified Data.Foldable as F

import Unsafe.Coerce

newtype List fs m a = Select { enumerate :: Has Weave fs m => Producer fs a m () }

instance Functor (List fs m) where
  fmap f (Select p) = Select (p //> (\x -> producer $ \yield -> yield (f x)))

instance Applicative (List fs m) where
  pure a = Select (producer $ \yield -> yield a)
  mf <*> mx = Select (
   for (enumerate mf) (\f ->
   for (enumerate mx) (\x ->
   (producer $ \yield -> yield (f x)))))

instance Monad (List fs m) where
  return a = Select (producer $ \yield -> yield a)
  m >>= f = Select (for (enumerate m) (\a -> enumerate (f a)))
  fail _ = mzero

instance Alternative (List fs m) where
  empty = Select (producer $ \_ -> return ())
  p1 <|> p2 = Select (weave $ \up dn -> do
     runWoven (enumerate p1) (unsafeCoerce up) (unsafeCoerce dn)
     runWoven (enumerate p2) (unsafeCoerce up) (unsafeCoerce dn))

instance MonadPlus (List fs m) where
  mzero = empty
  mplus = (<|>)

instance Monoid (List fs m a) where
  mempty = empty
  mappend = (<|>)

runList l = linearize (enumerate (l >> mzero))

each :: Has Weave fs m => F.Foldable f => f a -> Producer' fs a m ()
each xs = producer $ \yield ->
  F.foldr (\a p -> yield a >> p) (return ()) xs

discard :: Functor m => t -> Woven fs a' a b' b m ()
discard _ = Woven $ \_ _ -> return ()

every :: Has Weave fs m => List fs m a -> Producer' fs a m ()
every it = discard >\\ enumerate it


instance MTrans (List fs) where
  lift' m = Select (producer $ \yield -> do
    a <- lift m
    yield a
    )
