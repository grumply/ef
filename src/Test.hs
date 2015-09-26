{-
This implementation has a major drawback: performance. The worst-case
instruction, state, is 10x slower than mtl. But the trade-off is not all bad -
we gain the ability to pattern match on the tape pre-execution and modify
operations before they are run. This opens up the possiblity for various forms
of analysis and optimization. Also maintained is the ability to perform monadic
effects in both the tape and the computer.

-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExtendedDefaultRules #-}
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
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Functor.Identity

import Data.Functor

import Data.Maybe

import Control.Category

import Control.Monad

import Control.Monad.Trans.Free
import Control.Monad.Trans.Free.Church

import Prelude hiding ((.),id)

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
  Instr :: !(b -> a) -> f b -> Instrs fs a -> Instrs (f ': fs) a
instance Functor (Instrs is) where
  {-# INLINE fmap #-}
  fmap _ Empty = Empty
  fmap f (Instr ba fb fs) = Instr (f . ba) fb (fmap f fs)

{-# INLINE add #-}
add :: f a -> Instrs fs a -> Instrs (f ': fs) a
add fa Empty = Instr id fa Empty
add fa i = Instr id fa i

class Admits (x :: * -> *) (xs :: [* -> *]) where
  push :: x a -> Instrs xs a -> Instrs xs a
  pull :: Instrs xs a -> x a
instance Admits' x xs (IndexOf x xs) => Admits x xs where
  push = push' (Index :: Index (IndexOf x xs))
  pull = pull' (Index :: Index (IndexOf x xs))
class Admits' (x :: * -> *) (xs :: [* -> *]) (n :: Nat) where
  push' :: Index n -> x a -> Instrs xs a -> Instrs xs a
  pull' :: Index n -> Instrs xs a -> x a
instance (Functor x,xs ~ (x ': xs')) => Admits' x xs 'Z where
  push' _ xa (Instr _ _ fs) = Instr id xa fs
  pull' _ (Instr ba fb _) = fmap ba fb
instance (Admits' x xs' (IndexOf x xs')) => Admits' x (x' ': xs') ('S n) where
  push' _ xa (Instr ba fb xs) = Instr ba fb (push' (Index :: Index (IndexOf x xs')) xa xs)
  pull' _ (Instr _ _ xs) = pull' (Index :: Index (IndexOf x xs')) xs

newtype Instructions fs m = Instructions
  { getInstructions :: Instrs fs (Instructions fs m -> m (Instructions fs m)) }

{-# INLINE (*:*) #-}
(*:*) :: f a -> Instrs fs a -> Instrs (f ': fs) a
(*:*) = add
infixr 5 *:*

{-# INLINE view #-}
view :: (Uses x xs m) => Instructions xs m -> x (Instructions xs m -> m (Instructions xs m))
view = pull . getInstructions

-- {-# INLINE modify #-}
-- modify :: (Ins x fs, Ins y fs)
--        => (   x (Instructions fs m -> m (Instructions fs m))
--            -> y (Instructions fs m -> m (Instructions fs m)))
--        -> Instructions fs m -> Instructions fs m
-- modify f fs = let x = view fs in build $ push (f x) (getInstructions fs)

{-# INLINE instr #-}
instr :: (Uses x fs m)
      => x (Instructions fs m -> m (Instructions fs m))
      -> Instructions fs m -> m (Instructions fs m)
instr x = return . Instructions . push x . getInstructions

data Symbol (symbols :: [* -> *]) a where
  Symbol :: Denies s ss => (b -> a) -> s b -> Symbol (s ': ss) a
  Further :: Denies s ss => Symbol ss a -> Symbol (s ': ss) a
instance Functor (Symbol ss) where
  {-# INLINE fmap #-}
  fmap f (Symbol ba sb) = Symbol (f . ba) sb
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
  inj' _ = Symbol id
  prj' _ (Symbol ba sb) = Just (fmap ba sb)
  prj' _ (Further _) = Nothing
class Permits xs ys
instance Permits '[] ys
instance (Allows x ys,Permits xs ys) => Permits (x ': xs) ys

{-# INLINE symbol #-}
symbol :: (MonadFree (Symbol xs) m,Allows x xs) => x a -> m a
symbol = liftF . inj

class Pair f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r
instance Pair Identity Identity where
  {-# INLINE pair #-}
  pair f (Identity a) (Identity b) = f a b
instance Pair ((->) a) ((,) a) where
  {-# INLINE pair #-}
  pair p f g = uncurry (p . f) g
instance Pair ((,) a) ((->) a) where
  {-# INLINE pair #-}
  pair p (l,r) g = p r (g l)
instance Pair (Instrs '[]) (Symbol '[])
instance (Functor i,Functor s
         ,Pair i s
         ,Pair (Instrs is) (Symbol ss)
         ) => Pair (Instrs (i ': is)) (Symbol (s ': ss)) where
  {-# INLINE pair #-}
  pair p (Instr iba ib _) (Symbol sba sb) = pair p (fmap iba ib) (fmap sba sb)
  pair p (Instr _ _ is) (Further ss) = pair p is ss

{-# INLINE delta #-}
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

{-# INLINE run #-}

run :: (Monad m, Pair (Instrs is) (Symbol ss))
    => Instructions is m -> FT (Symbol ss) m r -> m (Instructions is m, r)
run is ss = delta is (fromFT ss)

type family Append (fs :: [k]) (gs :: [k]) :: [k] where
  Append '[] gs = gs
  Append fs '[] = fs
  Append (f ': fs) gs = f ': (Append fs gs)

class Append' fs gs hs where
  append :: (hs ~ (Append fs gs)) => Instrs fs a -> Instrs gs a -> Instrs hs a
instance Append' fs '[] fs where
  append fs _ = fs
instance Append' '[] gs gs where
  append _ gs = gs
instance (Append' fs gs h',h' ~ Append fs gs
         ,Append (f ': fs) gs ~ hs,hs ~ (f ': Append fs gs)) => Append' (f ': fs) gs hs where
  append (Instr ba fb fs') gs = Instr ba fb (append fs' gs)

class Draw (xs :: [* -> *]) (ys :: [* -> *]) where
  draw :: Instrs ys a -> Instrs xs a
instance Draw '[] ys where
  draw _ = Empty
instance (Functor x,Admits x ys,Draw xs ys) => Draw (x ': xs) ys where
  draw ys = Instr id (pull ys) (draw ys)

-- combine :: (Append fs gs ~ hs,Append' fs gs hs) => Instructions fs m -> Instructions gs m -> Instructions hs m
combine :: forall fs gs hs m.
           (Append' fs gs hs, Append fs gs ~ hs,Monad m,Subset fs hs,Subset gs hs)
        => Instructions fs m -> Instructions gs m -> Instructions hs m
combine fs gs = Instructions (append ((fmap liftTrans $ getInstructions fs) :: Instrs fs (Instructions hs m -> m (Instructions hs m)))
                                     ((fmap liftTrans $ getInstructions gs) :: Instrs gs (Instructions hs m -> m (Instructions hs m)))
                             )

class Subset fs gs where
  pullSubset :: Instrs gs a -> Instrs fs a
  pushSubset :: Instrs fs a -> Instrs gs a -> Instrs gs a
instance Subset '[] gs where
  pullSubset = const Empty
  pushSubset _ gs = gs
instance (Functor f,Subset fs gs,Admits f gs) => Subset (f ': fs) gs where
  pullSubset gs = Instr id (pull gs) (pullSubset gs)
  pushSubset (Instr ba fb fs) gs = pushSubset fs (push (fmap ba fb) gs)

liftTrans :: forall fs gs m. (Subset fs gs,Monad m)
          => (Instructions fs m -> m (Instructions fs m))
          -> Instructions gs m
          -> m (Instructions gs m)
liftTrans t gs = do
  let instrs = getInstructions gs
  fs' <- t (Instructions $ fmap _ (pullSubset instrs :: Instrs fs (Instructions gs m -> m (Instructions gs m))))
  return $ Instructions $ pushSubset (fmap liftTrans $ getInstructions fs') instrs

type Translation fs m = Instructions fs m -> m (Instructions fs m)
type Uses f fs m = (Admits' f fs (IndexOf f fs),Monad m)
type Has f fs m = (MonadFree (Symbol fs) m,Allows' f fs (IndexOf f fs))
type Instruction f fs m = f (Instructions fs m -> m (Instructions fs m))

--------------------------------------------------------------------------------
-- State

data State st k = Get (st -> k) | Put st k deriving Functor

get :: Has (State st) fs m => m st
get = symbol (Get id)

put :: Has (State st) fs m => st -> m ()
put st = symbol (Put st ())

modify :: Has (State st) fs m => (st -> st) -> m ()
modify f = get >>= (put . f)

--------------------------------------------------------------------------------
-- Store

data Store st k = Store st k (st -> k) deriving Functor

store :: (Uses (Store st) fs m) => st -> Instruction (Store st) fs m
store st = Store st return (instr . store)

state :: Monad m => st -> Instructions '[Store st] m
state st = Instructions (store st *:* Empty)

--------------------------------------------------------------------------------
-- State/Store pairing

instance Pair (Store st) (State st) where
  pair p (Store pos ret _) (Get stk)  = p ret (stk pos)
  pair p (Store _ _ seek) (Put st k) = p (seek st) k

data Ch = Ch Char
nextCh (Ch x) = Ch (succ x)
data I = I Int
nextI (I i) = I (succ i)

main = do
  (c,ci) <- delta (combine (state (Ch 'a')) (state (I 1))) $ do
             replicateM_ 100000 $ do
               modify nextCh
               modify nextI
             Ch c <- get
             I i <- get
             return (c,i)

  print ci
