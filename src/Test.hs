{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
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
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Main where

import Control.Monad.IO.Class

import Control.Monad
import Data.Coerce

data Nat = Z | S Nat
data Index (n :: Nat)= Index
type family IndexOf (f :: k) (fs :: [k]) :: Nat where
  IndexOf f (f ': fs) = 'Z
  IndexOf f (any ': fs) = 'S (IndexOf f fs)

type family Not (x :: k) (y :: k) :: Bool where
  Not x x = 'False
  Not x y = 'True
class Denies (x :: * -> *) (ys :: [* -> *])
instance Denies x '[]
instance (Denies x ys,Not x y ~ 'True) => Denies x (y ': ys)

data Instrs (is :: [* -> *]) a where
  Empty :: Instrs '[] a
  Instr :: Denies f fs => f a -> Instrs fs a -> Instrs (f ': fs) a

instance Functor (Instrs '[]) where
  {-# INLINE fmap #-}
  fmap _ Empty = Empty
instance (Functor f,Functor (Instrs fs)) => Functor (Instrs (f ': fs)) where
  {-# INLINE fmap #-}
  fmap f (Instr fa fs) = Instr (fmap f fa) (fmap f fs)

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

add :: (Denies f fs) => f a -> Instrs fs a -> Instrs (f ': fs) a
add fa Empty = Instr fa Empty
add fa i = Instr fa i

observe :: Instrs (f ': fs) a -> (f a,Instrs fs a)
observe (Instr fa fs) = (fa,fs)

(*:*) :: Denies f fs => f a -> Instrs fs a -> Instrs (f ': fs) a
(*:*) = add
infixr 5 *:*

newtype Instructions fs m = Instructions
  { getInstructions :: Instrs fs (Instructions fs m -> m (Instructions fs m)) }

{-# INLINE build #-}
build :: Instrs fs (Instructions fs m -> m (Instructions fs m)) -> Instructions fs m
build = coerce

{-# INLINE empty #-}
empty :: Instrs '[] a
empty = Empty

{-# INLINE single #-}
single :: f a -> Instrs '[f] a
single = (*:* empty)

{-# INLINE simple #-}
simple :: f (Instructions '[f] m -> m (Instructions '[f] m)) -> Instructions '[f] m
simple t = build $ single t

{-# INLINE view #-}
view :: Uses x xs m => Instructions xs m -> x (Instructions xs m -> m (Instructions xs m))
view xs = pull $ getInstructions xs

{-# INLINE instr #-}
instr :: Uses x fs m
      => Instruction x fs m
      -> Instructions fs m -> m (Instructions fs m)
instr x is = pure $ coerce $ push x $ coerce is

data Symbol (symbols :: [* -> *]) a where
  Symbol  :: Denies s ss => s a -> Symbol (s ': ss) a
  Further :: Denies s ss => Symbol ss a -> Symbol (s ': ss) a

instance Functor (Symbol '[])
instance (Functor s,Functor (Symbol ss)) => Functor (Symbol (s ': ss)) where
  {-# INLINE fmap #-}
  fmap f (Symbol sb) = Symbol (fmap f sb)
  fmap f (Further ss) = Further (fmap f ss)

class Allows x xs where
  inj :: x a -> Symbol xs a
  prj :: Symbol xs a -> Maybe (x a)
instance (Allows' x xs (IndexOf x xs)) => Allows x xs where
  inj x =
    let !n = Index :: Index (IndexOf x xs)
    in inj' n x
  prj xs =
    let !n = Index :: Index (IndexOf x xs)
    in prj' n xs

class Allows' x xs (n :: Nat) where
  inj' :: Index n -> x a -> Symbol xs a
  prj' :: Index n -> Symbol xs a -> Maybe (x a)
instance (Denies x' xs',xs ~ (x' ': xs'),Allows' x xs' (IndexOf x xs')) => Allows' x xs ('S n) where
  inj' _ xa = Further (inj' (Index :: Index (IndexOf x xs')) xa)
  prj' _ (Further ss) = prj' (Index :: Index (IndexOf x xs')) ss
instance (Denies x xs',xs ~ (x ': xs')) => Allows' x xs 'Z where
  inj' _ x = Symbol x
  prj' _ (Symbol sa) = Just sa
  prj' _ (Further _) = Nothing

class Permits xs ys
instance Permits '[] ys
instance (Allows x ys,Permits xs ys) => Permits (x ': xs) ys

class Pair f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r
instance Pair ((->) a) ((,) a) where
  pair p f g = uncurry (\x -> p (f x)) g
instance Pair ((,) a) ((->) a) where
  pair p (l,r) g = p r (g l)
instance Pair (Instrs '[]) (Symbol '[])
instance (Pair i s,Pair (Instrs is) (Symbol ss))
    => Pair (Instrs (i ': is)) (Symbol (s ': ss))
  where
    pair p (Instr ia _) (Symbol sa) = pair p ia sa
    pair p (Instr _ is) (Further ss) = pair p is ss

data Plan symbols m a where
  Stop :: a -> Plan symbols m a
  Step :: Symbol symbols x -> (x -> m (Plan symbols m a)) -> Plan symbols m a

instance Functor m => Functor (Plan symbols m) where
  {-# INLINE fmap #-}
  fmap f (Stop a) = Stop (f a)
  fmap f (Step symbols k) = Step symbols (\x -> fmap (fmap f) (k x))

instance Functor m => Applicative (Plan symbols m) where
  {-# INLINE pure #-}
  pure = Stop
  {-# INLINE (<*>) #-}
  (Stop ab) <*> (Stop a) = Stop (ab a)
  (Stop ab) <*> (Step symbols k) = Step symbols (fmap (fmap ab) <$> k)
  (Step symbols mab) <*> b = Step symbols $ fmap (<*> b) <$> mab

instance Functor m => Monad (Plan symbols m) where
  {-# INLINE return #-}
  return = Stop
  {-# INLINE (>>=) #-}
  Stop a >>= k = k a
  Step symbols k' >>= k = Step symbols (\x -> fmap (>>= k) (k' x))

data Handle

data I a where


i :: (MonadIO m,Has I fs m) => I a -> Plan fs m a
i ia = Step (inj ia) (pure . Stop)

{-# INLINE sym #-}
sym :: Has x symbols m => x a -> Plan symbols m a
sym xa = Step (inj xa) (\x -> pure $ Stop x)

{-# INLINE delta #-}
delta :: (Pair (Instrs is) (Symbol symbols),Monad m)
       => Instructions is m
       -> Plan symbols m r
       -> m (Instructions is m,r)
delta is (Stop result) = pure (is,result)
delta is (Step symbols k) = do
  let instrs = getInstructions is
  (trans,nxt) <- pair (curry pure) instrs symbols
  is' <- trans is
  nxt' <- k nxt
  delta is' nxt'

type Uses f fs m = (Applicative m,Admits' f fs (IndexOf f fs))
type Has f fs m = (Applicative m,Allows' f fs (IndexOf f fs))

type Instruction f fs m = f (Instructions fs m -> m (Instructions fs m))

data CC k
  = Set k
  | Call k
data Reify k = Reify k k

instance Pair Reify CC where
  pair p (Reify kl _) (Set k) = p kl k
  pair p (Reify _ kr) (Call k) = p kr k

reify :: Uses Reify fs m => Instruction Reify fs m
reify = Reify setter pure
  where
    setter fs = pure $ Instructions $ push (Reify setter (caller fs)) $ getInstructions fs
    caller fs = pure . const fs

data State st k
  = Get (st -> k)
  | Put st k

get :: Has (State st) fs m => Plan fs m st
get = sym (Get id)

put :: (Has (State st) fs m) => st -> Plan fs m ()
put st = sym (Put st ())

data Store st k = Store st k (st -> k)

instance Pair (Store st) (State st) where
  pair p (Store st k _  ) (Get stk ) = pair p (st,k) stk
  pair p (Store _  _ stk) (Put st k) = pair p stk (st,k)

store :: Uses (Store st) fs m => st -> Instruction (Store st) fs m
store st = Store st pure (instr . store)

modify :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify f = do
  st <- get
  put $ f st

modify' :: Has (State st) fs m => (st -> st) -> Plan fs m ()
modify' f = do
  st <- get
  put $! f st

isLazy :: Has (State Int) fs m => Plan fs m Int
isLazy = go 10000000
  where
    go (0 :: Int) = get
    go n = do
      modify' (+ (1 :: Int))
      go (n - 1)

main = do
  (c,i :: Int) <- delta (build $ store (1 :: Int) *:* reify *:* empty) (io (putStrLn "Test") >> isLazy)
  print i
  return ()
