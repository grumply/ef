{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE GADTs                     #-}

-- For Admits' and Allows'
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Main where

import Control.Monad.Trans.Free
import Control.Monad.Trans.Free.Church

type family Not (x :: k) (y :: k) :: Bool where
  Not x x = 'False
  Not x y = 'True

data Nat = Z | S Nat
data Index (n :: Nat)= Index

type family IndexOf (f :: k) (fs :: [k]) :: Nat where
  IndexOf f (f ': fs) = 'Z
  IndexOf f (any ': fs) = 'S (IndexOf f fs)
class Denies (x :: * -> *) (ys :: [* -> *])
instance Denies x '[]
instance (Denies x ys,Not x y ~ 'True) => Denies x (y ': ys)

data Instrs (is :: [* -> *]) a where
  Empty :: Instrs '[] a
  Instr :: f a -> Instrs fs a -> Instrs (f ': fs) a
instance Functor (Instrs '[]) where
  fmap _ Empty = Empty
instance (Functor f,Functor (Instrs fs)) => Functor (Instrs (f ': fs)) where
  fmap f (Instr fa fs) = Instr (fmap f fa) (fmap f fs)

add :: f a -> Instrs fs a -> Instrs (f ': fs) a
add fa Empty = Instr fa Empty
add fa i = Instr fa i

observe :: Functor f => Instrs (f ': fs) a -> (f a,Instrs fs a)
observe (Instr fa fs) = (fa,fs)

class Admits (x :: * -> *) (xs :: [* -> *]) where
  push :: x a -> Instrs xs a -> Instrs xs a
  pull :: Instrs xs a -> x a
instance Admits' x xs (IndexOf x xs) => Admits x xs where
  push = push' (Index :: Index (IndexOf x xs))
  pull = pull' (Index :: Index (IndexOf x xs))
class Admits' (x :: * -> *) (xs :: [* -> *]) (n :: Nat) where
  push' :: Index n -> x a -> Instrs xs a -> Instrs xs a
  pull' :: Index n -> Instrs xs a -> x a
instance (xs ~ (x ': xs')) => Admits' x xs 'Z where
  push' _ xa (Instr _ fs) = Instr xa fs
  pull' _ (Instr fa _) = fa
instance (Admits' x xs' (IndexOf x xs')) => Admits' x (x' ': xs') ('S n) where
  push' _ xa (Instr fa xs) = Instr fa (push' (Index :: Index (IndexOf x xs')) xa xs)
  pull' _ (Instr _ xs) = pull' (Index :: Index (IndexOf x xs')) xs

newtype Instructions fs m = Instructions
  { getInstructions :: Instrs fs (Instructions fs m -> m (Instructions fs m)) }

(*:*) :: f a -> Instrs fs a -> Instrs (f ': fs) a
(*:*) = add
infixr 5 *:*

view :: (Uses x xs m) => Instructions xs m -> x (Instructions xs m -> m (Instructions xs m))
view = pull . getInstructions

instr :: (Uses x fs m)
      => Instruction x fs m
      -> Instructions fs m -> m (Instructions fs m)
instr x = return . Instructions . push x . getInstructions

data Symbol (symbols :: [* -> *]) a where
  Symbol :: (Denies s ss,Functor s) => s a -> Symbol (s ': ss) a
  Further :: Denies s ss => Symbol ss a -> Symbol (s ': ss) a
instance Functor (Symbol ss) where
  fmap f (Symbol sb) = Symbol (fmap f sb)
  fmap f (Further ss) = Further (fmap f ss)
class Allows x xs where
  inj :: x a -> Symbol xs a
  prj :: Symbol xs a -> Maybe (x a)
instance (Allows' x xs (IndexOf x xs)) => Allows x xs where
  inj = inj' (Index :: Index (IndexOf x xs))
  prj = prj' (Index :: Index (IndexOf x xs))
class Allows' x xs (n :: Nat) where
  inj' :: Index n -> x a -> Symbol xs a
  prj' :: Index n -> Symbol xs a -> Maybe (x a)
instance (Denies x' xs',xs ~ (x' ': xs'),Allows' x xs' (IndexOf x xs')) => Allows' x xs ('S n) where
  inj' _ = Further . inj' (Index :: Index (IndexOf x xs'))
  prj' _ (Further ss) = prj' (Index :: Index (IndexOf x xs')) ss
instance (Functor x,Denies x xs',xs ~ (x ': xs')) => Allows' x xs 'Z where
  inj' _ = Symbol
  prj' _ (Symbol sb) = Just sb
  prj' _ (Further _) = Nothing
class Permits xs ys
instance Permits '[] ys
instance (Allows x ys,Permits xs ys) => Permits (x ': xs) ys

symbol :: (MonadFree (Symbol xs) m,Allows x xs) => x a -> m a
symbol = liftF . inj

class Pair f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r
instance Pair ((->) a) ((,) a) where
  pair p f g = uncurry (p . f) g
instance Pair ((,) a) ((->) a) where
  pair p (l,r) g = p r (g l)
instance Pair (Instrs '[]) (Symbol '[])
instance (Functor i,Functor s
         ,Pair i s
         ,Pair (Instrs is) (Symbol ss)
         ) => Pair (Instrs (i ': is)) (Symbol (s ': ss)) where
  pair p (Instr ia _) (Symbol sb) = pair p ia sb
  pair p (Instr _ is) (Further ss) = pair p is ss

delta :: (Pair (Instrs is) (Symbol ss),Monad m)
      => Instructions is m
      -> FreeT (Symbol ss) m r
      -> m (Instructions is m,r)
delta is ss = do
  s <- runFreeT ss
  let instrs = getInstructions is
  case s of
    Free sym -> do
      (trans,nxt) <- pair (curry return) instrs sym
      is' <- trans is
      delta is' nxt
    Pure result -> return (is,result)

using :: (Monad m, Pair (Instrs is) (Symbol ss))
    => Instructions is m -> FT (Symbol ss) m r -> m (Instructions is m, r)
using is ss = delta is (fromFT ss)

build = Instructions
empty = Empty
single = (*:* empty)
simple = build . single

type Uses f fs m = (Admits' f fs (IndexOf f fs),Monad m)
type Has f fs m = (MonadFree (Symbol fs) m,Allows' f fs (IndexOf f fs))
type Instruction f fs m = f (Instructions fs m -> m (Instructions fs m))

data State st k  = Get    (st -> k) | Put (st  , k) deriving Functor
data Store st k  = Store  (st  , k)       (st -> k) deriving Functor
data Store' st k = Store' !st k           (st -> k) deriving Functor

get :: Has (State st) fs m => m st
get = symbol (Get id)
put :: Has (State st) fs m => st -> m ()
put st = symbol (Put (st,()))
modify :: Has (State st) fs m => (st -> st) -> m ()
modify f = get >>= (put . f)

store :: (Uses (Store st) fs m) => st -> Instruction (Store st) fs m
store st = Store (st,return) (instr . store)

also :: Functor (Instrs fs)
     => (  (Instructions fs m -> m (Instructions fs m))
         -> Instructions fs m -> m (Instructions fs m))
     -> Instructions fs m
     -> Instructions fs m
also f = Instructions . fmap f . getInstructions

also' :: Monad m
      => (      Instrs fs (Instructions fs m -> m (Instructions fs m))
          -> m (Instrs fs (Instructions fs m -> m (Instructions fs m))))
      -> Instructions fs m
      -> m (Instructions fs m)
also' f fs0 = do
  let fs = getInstructions fs0
  fs' <- f fs
  return (Instructions fs')

instance Pair (Store st) (State st) where
  pair p (Store pos _) (Get stk)  = pair p pos stk
  pair p (Store _ seek) (Put stk) = pair p seek stk

newtype I = I Int
succI (I n) = I (succ n)

addN :: Has (State I) fs m => Int -> m I
addN = go
  where
    go 0 = get
    go n = modify succI >> go (n - 1)

main = do
  let st = simple $ store (I 0)
  (c,I ci) <- using st (addN 100000)
  print ci
