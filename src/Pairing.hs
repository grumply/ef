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

class (Functor f,Functor g) => Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance {-# OVERLAPPABLE #-} Pairing f g => Pairing g f where
  pair p f g = pair (flip p) g f

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance (Pairing f f', Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (Inl x) (Product a _) = pair p x a
  pair p (Inr x) (Product _ b) = pair p x b
