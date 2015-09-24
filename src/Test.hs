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
module Test where

import Data.Functor.Identity
import Data.Proxy

import Data.Bifunctor
import Data.Functor
import Data.Monoid

import Control.Arrow
import Control.Applicative hiding (empty)
import Control.Category
import Control.Comonad
-- import Control.Comonad.Cofree
import Control.Monad

import Control.Comonad.Store.Class
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free

import Control.Monad.Fix

import Control.Monad.Trans

import Prelude hiding ((.),id)

type family Not (x :: k) (y :: k) :: Bool where
  Not x x = 'False
  Not x y = 'True

data Nat = Z | S Nat
data Index (n :: Nat)= Index

type family IndexOf (f :: k) (fs :: [k]) :: Nat where
  IndexOf f (f ': fs) = Z
  IndexOf f (any ': fs) = S (IndexOf f fs)
class Denies (x :: * -> *) (ys :: [* -> *])
instance Denies x '[]
instance (Denies x ys,Not x y ~ 'True) => Denies x (y ': ys)

data Instrs (is :: [* -> *]) a where
  Empty :: Instrs '[] a
  Instr :: (b -> a) -> f b -> Instrs fs a -> Instrs (f ': fs) a
instance Functor (Instrs is) where
  fmap _ Empty = Empty
  fmap f (Instr ba fb fs) = Instr (f . ba) fb (fmap f fs)

add :: f a -> Instrs fs a -> Instrs (f ': fs) a
add fa Empty = Instr id fa Empty
add fa i = Instr id fa i

empty = Empty

class Admits (x :: * -> *) (xs :: [* -> *]) where
  push :: x a -> Instrs xs a -> Instrs xs a
  pull :: Instrs xs a -> x a
instance Admits' x xs (IndexOf x xs) => Admits x xs where
  push = push' (Index :: Index (IndexOf x xs))
  pull = pull' (Index :: Index (IndexOf x xs))
class Admits' (x :: * -> *) (xs :: [* -> *]) (n :: Nat) where
  push' :: Index n -> x a -> Instrs xs a -> Instrs xs a
  pull' :: Index n -> Instrs xs a -> x a
instance (Functor x,xs ~ (x ': xs')) => Admits' x xs Z where
  push' _ xa (Instr _ _ fs) = Instr id xa fs
  pull' _ (Instr ba fb _) = fmap ba fb
instance (Admits' x xs' (IndexOf x xs')) => Admits' x (x' ': xs') (S n) where
  push' _ xa (Instr ba fb xs) = Instr ba fb (push' (Index :: Index (IndexOf x xs')) xa xs)
  pull' _ (Instr _ _ xs) = pull' (Index :: Index (IndexOf x xs')) xs

type (:<) f fs = (Admits' f fs (IndexOf f fs))

newtype Instructions fs m = Instructions
  { getInstructions :: Instrs fs (Instructions fs m -> m (Instructions fs m)) }

build = Instructions

(*:*) = add
infixr 5 *:*

view :: (x :< xs) => Instructions xs m -> x (Instructions xs m -> m (Instructions xs m))
view = pull . getInstructions

using fs x = build $ x $ getInstructions fs

modify :: (x :< fs, y :< fs)
       => (   x (Instructions fs m -> m (Instructions fs m))
           -> y (Instructions fs m -> m (Instructions fs m)))
       -> Instructions fs m -> Instructions fs m
modify f fs = let x = view fs in build $ push (f x) (getInstructions fs)

instr :: (Monad m, x :< fs)
      => x (Instructions fs m1 -> m1 (Instructions fs m1))
      -> Instructions fs m1 -> m (Instructions fs m1)
instr x = return . build . push x . getInstructions

data Symbol (symbols :: [* -> *]) a where
  Symbol :: Denies s ss => (b -> a) -> s b -> Symbol (s ': ss) a
  Further :: Denies s ss => Symbol ss a -> Symbol (s ': ss) a
instance Functor (Symbol ss) where
  fmap f (Symbol ba sb) = Symbol (f . ba) sb
  fmap f (Further ss) = Further (fmap f ss)
class Allows x xs where
  inj :: x a -> Symbol xs a
  prj :: Symbol xs a -> x a
instance (Allows' x xs (IndexOf x xs)) => Allows x xs where
  inj = inj' (Index :: Index (IndexOf x xs))
  prj = prj' (Index :: Index (IndexOf x xs))
class Allows' x xs (n :: Nat) where
  inj' :: Index n -> x a -> Symbol xs a
  prj' :: Index n -> Symbol xs a -> x a
instance (Denies x' xs',xs ~ (x' ': xs'),Allows' x xs' (IndexOf x xs')) => Allows' x xs (S n) where
  inj' _ = Further . inj' (Index :: Index (IndexOf x xs'))
  prj' _ (Further ss) = prj' (Index :: Index (IndexOf x xs')) ss
instance (Functor x,Denies x xs',xs ~ (x ': xs')) => Allows' x xs Z where
  inj' _ = Symbol id
  prj' _ (Symbol ba sb) = fmap ba sb
class Permits xs ys
instance Permits '[] ys
instance (Allows x ys,Permits xs ys) => Permits (x ': xs) ys

symbol :: (MonadFree (Symbol xs) m,Allows x xs) => x a -> m a
symbol = liftF . inj

class Rebuildable x fs fs' where
  rebuild :: x fs a -> x fs' a
instance Rebuildable Instrs xs '[] where
  rebuild _ = Empty
instance (y :< xs,Rebuildable Instrs xs ys) => Rebuildable Instrs xs (y ': ys) where
  rebuild is = Instr id (pull is) (rebuild is)

rearrange :: (Pair (Instrs gs') (Symbol fs),Rebuildable Instrs gs gs')
               => Symbol fs a -> Instrs gs m -> Instrs gs' m
rearrange ss is = rebuild is

class Pair f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r
instance Pair Identity Identity where
  pair f (Identity a) (Identity b) = f a b
instance Pair ((->) a) ((,) a) where
  pair p f g = uncurry (p . f) g
instance Pair ((,) a) ((->) a) where
  pair p (l,r) g = p r (g l)
instance (Functor i,Functor s
         ,Pair i s
         ,Pair (Instrs is) (Symbol ss)
         ) => Pair (Instrs (i ': is)) (Symbol (s ': ss)) where
  pair p (Instr iba ib _) (Symbol sba sb) = pair p (fmap iba ib) (fmap sba sb)
  pair p (Instr _ _ is) (Further ss) = pair p is ss


type Tape ss = FreeT (Symbol ss)
type Computer is = Instructions is

delta :: (Pair (Instrs is) (Symbol ss),Monad m)
      => Computer is m
      -> Tape ss m r
      -> m (Computer is m,r)
delta is ss = do
  s <- runFreeT ss
  case s of
    Free symbol -> _
    Pure result -> _

data A a = A Int a
  deriving Functor
data B a = B Char a
  deriving Functor

ab = build $ (B 'b' b) *:* (A 3 a) *:* empty
  where
    a = instr (B 'a' a)
    b = instr (A 2 b)

runA :: (A :< fs,Monad m)
     => Instructions fs m -> m (Int,Instructions fs m)
runA fs = do
  let A i t = view fs
  fs' <- t fs
  return (i,fs')

runB :: (B :< fs,Monad m) => Instructions fs m -> m (Char,Instructions fs m)
runB fs = do
  let B c t = view fs
  fs' <- t fs
  return (c,fs')

main = do
  (c,ab0) <- runA ab
  (i,_) <- runB ab0
  print (i,c)
