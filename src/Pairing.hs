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

import           Control.Parallel

class (Functor f,Functor g) => Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance {-# OVERLAPPABLE #-} Pairing f g => Pairing g f where
  pair p f g = pair (flip p) g f

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = a `par` b `pseq` f a b

instance Pairing ((->) a) ((,) a) where
  pair p f g = f `par` g `pseq` uncurry (p . f) g

instance (Pairing f f', Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (Inl l) (Product a _) = l `par` a `pseq` pair p l a
  pair p (Inr r) (Product _ b) = r `par` b `pseq` pair p r b
