{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pairing (Pairing(..)) where


import Control.Applicative
import           Control.Comonad              (Comonad, extract, cfix)
import           Control.Comonad.Trans.Cofree (CofreeT, unwrap, coiterT)
import           Control.Monad.Trans.Free     (FreeF (..), FreeT, runFreeT, iterT)
import           Data.Functor.Identity        (Identity (..))
import qualified Control.Monad.Free as Free
import qualified Control.Comonad.Cofree as Cofree

import           Sum
import           Product

import           Control.Category
import           Prelude hiding ((.),id)

class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r

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
