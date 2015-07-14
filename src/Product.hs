{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE DeriveFunctor          #-}
module Product (
      (:+:)(..)
    , (:<:)
    , (:*:)(..)
    , (*:*)
    , inj, prj
    ) where

import           Pairing        (Pairing (..))

import           Control.Applicative (liftA2)

import           Data.Proxy

data (f :+: g) x = Inl (f x) | Inr (g x)
  deriving Functor

data Crumbs = Here | L Crumbs | R Crumbs

data Res = Found Crumbs | NotFound | Ambiguous

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

data (f :*: g) a = Product (f a) (g a)
  deriving Functor

(*:*) :: (Functor f,Functor g) => (a -> f a) -> (a -> g a) -> a -> (f :*: g) a
(*:*) = liftA2 Product

instance (Pairing f f',Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (Inl x) (Product a _) = pair p x a
  pair p (Inr x) (Product _ b) = pair p x b
instance (Pairing f f',Pairing g g') => Pairing (f :*: g) (f' :+: g') where
  pair p (Product a _) (Inl x) = pair p a x
  pair p (Product _ b) (Inr x) = pair p b x
