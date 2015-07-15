{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
module Subsumption ((:<:), inj, prj) where

import           Sum

import           Data.Proxy

data Crumbs = Here | L Crumbs | R Crumbs

data Res = Found Crumbs | NotFound | Ambiguous

-- I suspect a 'type familiy Elem e l r' with :+: defined in terms of a
-- terminating Void type would get rid of the undecidable instances.
type family Elem e f :: Res where
  Elem e e         = 'Found 'Here
  Elem e (l :+: r) = Choose (Elem e l) (Elem e r)
  Elem e f         = 'NotFound

type family Choose e f :: Res where
  Choose ('Found x) ('Found y) = 'Ambiguous
  Choose 'Ambiguous x = 'Ambiguous
  Choose x 'Ambiguous = 'Ambiguous
  Choose ('Found a) b = 'Found (L a)
  Choose a ('Found b) = 'Found (R b)
  Choose a b = 'NotFound

class (Functor f,Functor g) => Subsume (res :: Res) f g where
  inj' :: Proxy res -> f a -> g a
  prj' :: Proxy res -> g a -> Maybe (f a)

instance Functor f => Subsume ('Found 'Here) f f where
  inj' _ = id
  prj' _ = Just

instance (Functor f,Functor l,Functor r,Subsume ('Found p) f l)
  => Subsume ('Found ('L p)) f (l :+: r) where
  inj' _ = Inl . inj' (Proxy :: Proxy ('Found p))
  prj' _ (Inl x) = prj' (Proxy :: Proxy ('Found p)) x
  prj' _ _       = Nothing

instance (Functor f,Functor l,Functor r,Subsume ('Found p) f r)
  => Subsume ('Found ('R p)) f (l :+: r) where
  inj' _ = Inr . inj' (Proxy :: Proxy ('Found p))
  prj' _ (Inr x) = prj' (Proxy :: Proxy ('Found p)) x
  prj' _ _       = Nothing

inj :: forall f g a. (Functor f,Functor g,f :<: g) => f a -> g a
inj = inj' (Proxy :: Proxy (Elem f g))

prj :: forall f g a. (Functor f,Functor g,f :<: g) => g a -> Maybe (f a)
prj = prj' (Proxy :: Proxy (Elem f g))

type f :<: g = Subsume (Elem f g) f g

infixl 5 :<:
