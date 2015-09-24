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

data Nat = Z | S Nat
data Index (n :: Nat)= Index

type family IndexOf (f :: k) (fs :: [k]) :: Nat where
  IndexOf f (f ': fs) = Z
  IndexOf f (any ': fs) = S (IndexOf f fs)

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

get = pull . getInstructions
using fs x = Instructions $ x $ getInstructions fs

data A a = A Int a
  deriving Functor
data B a = B Char a
  deriving Functor

build = Instructions
(*:*) = add
infixr 5 *:*
instr x = return . Instructions . push x . getInstructions

ab = build $ (B 'b' b) *:* (A 3 a) *:* empty
  where
    a = instr (B 'a' a)
    b = instr (A 2 b)

runA :: (A :< fs,Monad m)
     => Instructions fs m -> m (Int,Instructions fs m)
runA fs = do
  let A i t = get fs
  fs' <- t fs
  return (i,fs')

runB :: (B :< fs,Monad m) => Instructions fs m -> m (Char,Instructions fs m)
runB fs = do
  let B c t = get fs
  fs' <- t fs
  return (c,fs')

main = do
  (c,ab0) <- runA ab
  (i,_) <- runB ab0
  print (i,c)




unfoldM :: Monad m
        => Instructions fs m
        -> w (Instructions fs m -> m (Instructions fs m))
        -> m (CofreeT (Instrs fs) w (Instructions fs m -> m (Instructions fs m)))
unfoldM instrs wt = do
  let t = extract wt
  return (CofreeT (extend (\_ -> t :< _) wt))
