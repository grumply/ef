> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeOperators #-}
> module Product (
>     (:+:)(..)
>   , (:<:)(..)
>   , (:*:)(..)
>   , (*:*)
>   , Product(..)
>   , Sum(..)
>   ) where

> import Pairing

> import Control.Applicative (liftA2)

> data Sum f g a = InL (f a) | InR (g a)
>   deriving (Functor)

> type f :+: g = Sum f g

> infixr 1 :+:

> class (Functor sub,Functor sup) => sub :<: sup where
>   inj :: sub a -> sup a

> instance Functor f => f :<: f where
>   inj = id

> instance (Functor f,Functor g) => f :<: (f :+: g) where
>   inj = InL

> instance {-# OVERLAPS #-} (Functor f,Functor g,Functor h,f :<: g) => f :<: (h :+: g) where
>   inj = InR . inj

> data Product f g a = Product (f a) (g a)
>   deriving (Functor)

> type f :*: g = Product f g

> infixr 1 :*:

> (*:*) :: (Functor f,Functor g) => (a -> f a) -> (a -> g a) -> (a -> (f :*: g) a)
> (*:*) = liftA2 Product

> infixr 1 *:*

> instance (Pairing f f',Pairing g g') => Pairing (f :+: g) (f' :*: g') where
>   pair p (InL x) (Product a _) = pair p x a
>   pair p (InR x) (Product _ b) = pair p x b

> instance (Pairing f f',Pairing g g') => Pairing (f :*: g) (f' :+: g') where
>   pair p (Product a _) (InL x) = pair p a x
>   pair p (Product _ b) (InR x) = pair p b x
