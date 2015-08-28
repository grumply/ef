{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleContexts       #-}
module Pairing ( Pairing(..) ) where

import           Data.Functor.Identity        (Identity (..))

import           Sum
import           Product

class (Functor f,Functor g) => Pairing f g | f -> g where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f g = uncurry (p . f) g

instance Pairing ((,) a) ((->) a) where
  pair p (l,r) g = p r (g l)

instance (Pairing f f', Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (Inl l) (Product a _) = pair p l a
  pair p (Inr r) (Product _ b) = pair p r b
