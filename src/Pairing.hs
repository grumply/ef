{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pairing (
      Pairing(..)
    , PairingM(..)
    , pairEffect
    , pairEffect'
    , pairEffectM
    , pairEffectM'
    ) where

import Control.Applicative
import           Control.Comonad              (Comonad, extract)
import           Control.Comonad.Trans.Cofree (CofreeT, unwrap)
import           Control.Monad.Trans.Free     (FreeF (..), FreeT, runFreeT)
import           Data.Functor.Identity        (Identity (..))
import qualified Control.Monad.Free as Free
import qualified Control.Comonad.Cofree as Cofree

import           Sum
import           Product

import           Control.Category
import           Prelude hiding ((.),id)


class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance {-# OVERLAPPABLE #-}Pairing f g => Pairing g f where
  pair p f g = pair (flip p) g f

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))

instance (Pairing f f', Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (Inl x) (Product a _) = pair p x a
  pair p (Inr x) (Product _ b) = pair p x b

instance (Pairing f f', Pairing g g') => Pairing (f :*: g) (f' :+: g') where
  pair p (Product a _) (Inl x) = pair p a x
  pair p (Product _ b) (Inr x) = pair p b x


pairEffect
  :: (Monad m, Cofree.ComonadCofree f w, Pairing f g) =>
     (a1 -> a -> r) -> w a1 -> FreeT g m a -> m r
pairEffect p s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> return $ p (extract s) x
    Free gs -> pair (pairEffect p) (unwrap s) gs


pairEffect'
  :: (Monad m, Cofree.ComonadCofree f w, Pairing f g) =>
     (t -> a -> r) -> w (m t) -> FreeT g m a -> m r
pairEffect' p s c = do
  a  <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> return $ p a x
    Free gs -> pair (pairEffect' p) (unwrap s) gs

class (Functor m, Functor f, Functor g) => PairingM f g m | f g -> m where
  pairM :: (a -> b -> m r) -> f a -> g b -> m r

pairEffectM
  :: (Monad m, Comonad w, PairingM f g m) =>
     (a -> b -> m r) -> CofreeT f w a -> FreeT g m b -> m r
pairEffectM p s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> p (extract s) x
    Free gs -> pairM (pairEffectM p) (unwrap s) gs

pairEffectM'
  :: (Monad m, Cofree.ComonadCofree f w, PairingM f g m) =>
     (a -> b -> m r) -> w (m a) -> FreeT g m b -> m r
pairEffectM' p s c = do
  a  <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> p a x
    Free gs -> pairM (pairEffectM' p) (unwrap s) gs
