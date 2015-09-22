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
{-# LANGUAGE DefaultSignatures #-}
module Turing where

import Data.Functor.Identity
import Data.Proxy

import Data.Bifunctor
import Data.Functor
import Data.Monoid

import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad

import Control.Comonad.Store.Class
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free

import Control.Monad.Fix

import Control.Monad.Trans

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

symbol :: (MonadFree (Symbols xs) m,Allows x xs) => x a -> m a
symbol = liftF . inj

build :: Comonad w
      => (w (Instructions fs a) -> Instructions fs (w (Instructions fs a)))
      -> w (Instructions fs a)
      -> Computer fs w a
build = coiterT

embed :: (Comonad w,Functor f) => w (f a) -> f (w (f a))
embed w = fmap (const w) (extract w)

construct :: Comonad w => w (Instructions fs b) -> Computer fs w b
construct = build embed

deconstruct :: Comonad w => Computer fs w b -> w (Instructions fs b)
deconstruct = extend (headF . extract) . runCofreeT

decontextualize :: (Functor f,Comonad w) => f (w a) -> f a
decontextualize = fmap extract

look :: Comonad w => Computer fs w a -> w (Instructions fs (Computer fs w a))
look = extend (tailF . extract) . runCofreeT

inject :: (Comonad w, Admits f fs)
       => (w (Instructions fs a) -> f (w (Instructions fs a)))
       -> (w (Instructions fs a) -> Instructions fs (w (Instructions fs a)))
inject f w = push (f w) (embed w)

reconstruct :: (Comonad w, Admits x xs)
        => (w (Instructions xs a) -> x a)
        -> w (Instructions xs a)
        -> w (Instructions xs a)
reconstruct f w = fmap (push (f w)) w

hm :: (Comonad w, Admits x xs, Transform (w (Instructions xs a1) -> w (Instructions xs a1)) w a)
   => (w (Instructions xs a1) -> x a1) -> w a -> w a
hm = trans . reconstruct

untied :: Functor f => f a -> f a
untied = fmap id

tied :: (Functor f) => (f (f a -> a)) -> f a
tied x = go where go = fmap ($ go) x

refs :: (Functor f) => f (f (f a -> a) -> f a -> a) -> f a
refs = tied . tied

something :: (Applicative f, Comonad w) => w (f (f b -> b)) -> f (f b)
something x = go where go = fmap (<*> go) (fmap extract $ embed x)

somethingElse :: (Applicative f, Comonad w) => f (w (f (f b -> b))) -> f (f b)
somethingElse x = go where go = fmap (<*> go) (fmap extract x)

somethingOther :: (Applicative f, Monoid (f (f a))) => f (f (f a) -> f a) -> f (f (f a))
somethingOther x = go where go = fmap (<*> go) (fmap embed x)

other :: (Applicative f, Comonad w) => f (w (f (f b -> b))) -> f (f (f b))
other = fmap something


-- fix $ \xs -> fmap ($ xs) fs

-- fix $ (<@> fs) . fmap (fmap . flip ($)) . duplicate


y :: (Functor f, Comonad w) => f (w (f a -> a)) -> f a
y = tied . decontextualize

z :: (Functor f, Monoid (f (f a))) => (f (f a) -> f a) -> f (f a)
z = tied . embed

z'
  :: Comonad w =>
     ((w (Instructions fs a1) -> Computer fs w a1)
      -> ((w (Instructions fs a1) -> Instructions fs (w (Instructions fs a1))) -> a)
      -> a
     )
     -> (w (Instructions fs a1) -> Instructions fs (w (Instructions fs a1)))
     -> a
z' = tied . (. build)

z'' :: (Comonad w, Admits' f fs (IndexOf f fs))
    => ((w (Instructions fs a1) -> Instructions fs (w (Instructions fs a1)))
         -> ((w (Instructions fs a1) -> f (w (Instructions fs a1))) -> a)
         -> a
       )
       -> (w (Instructions fs a1) -> f (w (Instructions fs a1)))
       -> a
z'' = tied . (. inject)

z'''
  :: (Comonad w, Admits' x xs (IndexOf x xs)) =>
     ((w (Instructions xs a1) -> w (Instructions xs a1))
      -> ((w (Instructions xs a1) -> x a1) -> a) -> a)
     -> (w (Instructions xs a1) -> x a1) -> a
z''' = tied . (. reconstruct)

data Get st k = Get (st -> k)
get :: (MonadFree (Symbols xs) m, Allows' (Get a) xs (IndexOf (Get a) xs)) => m a
get = symbol (Get id)
data Put st k = Put st k
put :: (MonadFree (Symbols xs) m,Allows' (Put st) xs (IndexOf (Put st) xs)) => st -> m ()
put a = symbol (Put a ())

data Store st k = Store st (st -> k)
  deriving Functor

type Computer fs w a = CofreeT (Instructions fs) w (Instructions fs a)
-- type Computer fs w m a = CofreeT (Instructions fs) w (m (a,Instructions fs a))


store :: (Comonad w, Admits' (Store st) fs (IndexOf (Store st) fs))
      => st -> w (Instructions fs a) -> Store st (w (Instructions fs a))
store st wa = Store st (\new -> _)

x :: (Functor f, Transform (f a) f a) => f (f a -> a) -> f a -> f a
x = trans . tied

class Transform t c a where
  trans :: t -> (c a -> c a)
  default trans :: (c a -> c a) -> (c a -> c a)
  trans = id
instance Monoid (m a) => Transform (m a) m a where
  trans = (<>)
instance Alternative f => Transform (f a) f a where
  trans = (<|>)
instance Functor f     => Transform (a -> a) f a where
  trans = fmap -- map
instance Comonad w     => Transform (w a -> a) w a where
  trans = (<<=) -- extend
instance Monad m       => Transform (a -> m a) m a where
  trans = (=<<) -- bind
instance Applicative f => Transform (f (a -> a)) f a where
  trans = (<*>) -- apply
instance (Applicative f,Monad f) => Transform (f (a -> f a)) f a where
  trans f fa = join (f <*> fa)
instance (Applicative f,Comonad f) => Transform (f (f a -> a)) f a where
  trans f fa = extract f <<= fa
instance (Comonad w,Functor f) => Transform (w (f a -> f a)) f a where
  trans = extract
instance Category c    => Transform (c a a) (c a) a where
  trans = (.) -- compose

-- store :: (Comonad w,Admits (Store st) fs)
--       => st
--       -> (Instructions fs a -> w (m (Instructions fs a)))
--       -> Store st (w (m (Instructions fs a)))
-- store st wa = Store st (\st' -> extend (\wa' -> let fs = extract wa'
--                                                     Store _ k = pull fs
--                                                 in push (Store st' k) fs
--                                        ) wa)


-- coGet :: Admits Identity fs => st -> Computer fs m a -> Computer (CoGet st ': fs) m a
-- coGet st = hoistComputer (_ (CoGet st _))
