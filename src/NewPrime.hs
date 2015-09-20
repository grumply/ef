{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE InstanceSigs              #-}
module Turing where

import Data.Functor.Identity
import Data.Proxy

import Data.Bifunctor
import Data.Functor
import Data.Monoid

import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Monad

import Prelude hiding ((.),id)

{-
section :: Comonad f => f a -> Cofree f a
coiter :: Functor f => (a -> f a) -> a -> Cofree f a
unfold :: Functor f => (b -> (a, f b)) -> b -> Cofree f a
unfoldM :: (Traversable f, Monad m) => (b -> m (a, f b)) -> b -> m (Cofree f a)
hoistCofree :: Functor f => (forall x. f x -> g x) -> Cofree f a -> Cofree g a
hoistCofree f (x :< y) = x :< f (hoistCofree f <$> y)

-}

-- coGet :: Denies (CoGet st) fs
--       => st -> Computer fs m -> Computer (CoGet st ': fs) m
-- coGet st = hoistComputer (\is -> undefined)

-- coPut :: (Denies (CoPut st) fs,Allows (CoGet st) fs)
--       => Computer fs m -> Computer (CoPut st ': fs) m
-- coPut = hoistComputer (\is -> undefined)

data Nat = Z | S Nat
data Index (n :: Nat) = Index

type family IndexOf (f :: k) (fs :: [k]) :: Nat where
  IndexOf f (f ': fs) = Z
  IndexOf f (any ': fs) = S (IndexOf f fs)

type family Not (x :: k) (y :: k) :: Bool where
  Not x x = 'False
  Not x y = 'True

type family Is (x :: k) (y :: k) :: Bool where
  Is x x = 'True
  Is x y = 'False

type family Remove (x :: k) (xs :: [k]) :: [k] where
  Remove x '[] = '[]
  Remove x (x ': xs') = Remove x xs'
  Remove x (y ': xs') = y ': (Remove x xs')

class Denies (x :: * -> *) (ys :: [* -> *])
instance Denies x '[]
instance (Denies x ys,Not x y ~ 'True) => Denies x (y ': ys)
class Refuses (xs :: [* -> *]) (ys :: [* -> *])
instance Refuses '[] ys
instance (Denies x ys,Refuses xs ys) => Refuses (x ': xs) ys

data Instructions (is :: [* -> *]) a where
  Empty :: Instructions '[] a
  Row :: (b -> a) -> f b -> Instructions fs a -> Instructions (f ': fs) a
view :: Functor x => Instructions (x ': xs) a -> (x a,Instructions xs a)
view (Row ab b xs) = (fmap ab b,xs)

instance Functor (Instructions is) where
  fmap f (Row ba fb cs) = Row (f . ba) fb (fmap f cs)
  fmap _ Empty = Empty
class Admits (x :: * -> *) (xs :: [* -> *]) where
  push :: x a -> Instructions xs a -> Instructions xs a
  pull :: Functor x => Instructions xs a -> x a
instance Admits' x xs (IndexOf x xs) => Admits x xs where
  pull = pull' (Index :: Index (IndexOf x xs))
  push = push' (Index :: Index (IndexOf x xs))
class Admits' (x :: * -> *) (xs :: [* -> *]) (n :: Nat) where
  pull' :: Index n -> Instructions xs a -> x a
  push' :: Index n -> x a -> Instructions xs a -> Instructions xs a
instance (Functor x,xs ~ (x ': xs')) => Admits' x xs Z where
  pull' _ (Row ba fb _) = fmap ba fb
  push' _ xa (Row _ _ xs) = Row id xa xs
instance (Functor x,xs ~ (x' ': xs'),Admits' x xs' (IndexOf x xs')) => Admits' x xs (S n) where
  pull' _ (Row _ _ xs) = pull' (Index :: Index (IndexOf x xs')) xs
  push' _ xa (Row ba fb xs) = Row ba fb (push' (Index :: Index (IndexOf x xs')) xa xs)

-- requires functor
class Draw (xs :: [* -> *]) (ys :: [* -> *]) where
  draw :: Instructions ys a -> Instructions xs a
instance Draw '[] ys where
  draw _ = Empty
instance (Functor x,Admits x ys,Draw xs ys) => Draw (x ': xs) ys where
  draw ys = Row id (pull ys) (draw ys)

-- requires functor
class Substitute (xs :: [* -> *]) (ys :: [* -> *]) where
  merge :: Instructions xs a -> Instructions ys a -> Instructions ys a
instance Substitute '[] ys where
  merge _ ys = ys
instance (Functor x,Admits' x ys (IndexOf x ys),Substitute xs ys) => Substitute (x ': xs) ys where
  merge xs ys =
    let (x,xs') = view xs
    in merge xs' (push x ys)

-- requires functor
class Rebuild (xs :: [* -> *]) (ys :: [* -> *]) where
  rebuild :: Instructions xs a -> Instructions ys a
instance Rebuild xs '[] where
  rebuild _ = Empty
instance (Functor y,Admits' y xs (IndexOf y xs),Rebuild xs ys') => Rebuild xs (y ': ys') where
  rebuild is = Row id (pull is) (rebuild is)

add :: Admits Identity fs => (a -> f a) -> Instructions fs a -> Instructions (f ': fs) a
add afa is = Row id (afa (runIdentity (pull is))) is
sub :: (Admits f fs,Denies f fs',Draw fs' fs) => Proxy f -> Instructions fs a -> Instructions fs' a
sub _ = draw

close :: (Admits Identity fs, Denies Identity fs', Draw fs' fs) => Instructions fs a -> Instructions fs' a
close = sub (Proxy :: Proxy Identity)


{-
data FEFree r a where
  Pure :: a → FEFree r a
  Impure :: Union r x → (x → FEFree r a) → FEFree r a
-}


-- consider this type for computer as well:
-- type Computer fs gs w m a
--   = forall x. CofreeT (Instructions fs) w
--                 (m (Instructions fs x -> Instructions fs x)
--                 ,FreeT (Symbols gs) m a)



data Symbols (symbols :: [* -> *]) a where
  Symbol :: Denies s ss => (b -> a) -> s b -> Symbols (s ': ss) a
  Further :: Denies s ss => Symbols ss a -> Symbols (s ': ss) a
instance Functor (Symbols ss) where
  fmap f (Symbol ba sb) = Symbol (f . ba) sb
  fmap f (Further ss) = Further (fmap f ss)
class Allows x xs where
  inj :: x a -> Symbols xs a
  prj :: Functor x => Symbols xs a -> x a
instance (Allows' x xs (IndexOf x xs)) => Allows x xs where
  inj = inj' (Index :: Index (IndexOf x xs))
  prj = prj' (Index :: Index (IndexOf x xs))
class Allows' x xs (n :: Nat) where
  inj' :: Index n -> x a -> Symbols xs a
  prj' :: Functor x => Index n -> Symbols xs a -> x a
instance (Denies x' xs',xs ~ (x' ': xs'),Allows' x xs' (IndexOf x xs')) => Allows' x xs (S n) where
  inj' _ = Further . inj' (Index :: Index (IndexOf x xs'))
  prj' _ (Further ss) = prj' (Index :: Index (IndexOf x xs')) ss
instance (Denies x xs',xs ~ (x ': xs')) => Allows' x xs Z where
  inj' _ = Symbol id
  prj' _ (Symbol ba sb) = fmap ba sb
class Permits xs ys
instance Permits '[] ys
instance (Allows x ys,Permits xs ys) => Permits (x ': xs) ys

data Tape (fs :: [* -> *]) a where
  Result :: (b -> a) -> b -> Tape fs a
  Step :: (x -> Tape fs a) -> Symbols fs x -> Tape fs a
instance Functor (Tape fs) where
  fmap f (Result ba b) = Result (f . ba) b
  fmap f (Step g syms) = Step (fmap f . g) syms
instance Applicative (Tape fs) where
  pure = Result id
  (Result ba b) <*> (Result ba' b') = Result id ((ba b) (ba' b'))
  (Result ba b) <*> (Step g syms) = Step (fmap (fmap (ba b)) g) syms
  (Step g syms) <*> step = Step (fmap (<*> step) g) syms
instance Monad (Tape fs) where
  return = pure
  (Result ba b) >>= f = f (ba b)
  (Step g syms) >>= f = Step (fmap (>>= f) g) syms

symbol :: (Allows x xs) => x a -> Tape xs a
symbol = Step (Result id) . inj

data Computer f m a where
  Plan :: (b -> m a) -> b
       -> (Instructions g (Computer g m b) -> Computer f m a)
       -> Instructions g (Computer g m b)
       -> Computer f m a


-- data Get st k = Get (st -> k)
-- get :: Allows (Get a) xs => Tape xs a
-- get = symbol (Get id)
-- data Put st k = Put st k
-- put :: Allows (Put a) xs => a -> Tape xs ()
-- put a = symbol (Put a ())

-- data CoGet st k = CoGet st k
-- data CoPut st k = CoPut (st -> k)

-- coGet :: Admits Identity fs => st -> Computer fs m a -> Computer (CoGet st ': fs) m a
-- coGet st = hoistComputer (_ (CoGet st _))
