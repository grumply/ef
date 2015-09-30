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
module Mop where

import Control.Monad
import Data.Bifunctor
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
  fmap _ Empty = Empty
instance (Functor f,Functor (Instrs fs)) => Functor (Instrs (f ': fs)) where
  fmap f (Instr fa fs) = Instr (fmap f fa) (fmap f fs)

class Admits (x :: * -> *) (xs :: [* -> *]) where
  push :: x a -> Instrs xs a -> Instrs xs a
  pull :: Instrs xs a -> x a
instance Admits' x xs (IndexOf x xs) => Admits x xs where
  push xa = push' (Index :: Index (IndexOf x xs)) xa
  pull xs = pull' (Index :: Index (IndexOf x xs)) xs

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

data Symbol (symbols :: [* -> *]) a where
  Symbol  :: Denies s ss => s a -> Symbol (s ': ss) a
  Further :: Denies s ss => Symbol ss a -> Symbol (s ': ss) a

instance Functor (Symbol '[])
instance (Functor s,Functor (Symbol ss)) => Functor (Symbol (s ': ss)) where
  fmap f (Symbol sb) = Symbol (fmap f sb)
  fmap f (Further ss) = Further (fmap f ss)

class Allows x xs where
  inj :: x a -> Symbol xs a
  prj :: Symbol xs a -> Maybe (x a)
instance (Allows' x xs (IndexOf x xs)) => Allows x xs where
  inj xa = inj' (Index :: Index (IndexOf x xs)) xa
  prj xs = prj' (Index :: Index (IndexOf x xs)) xs

class Allows' x xs (n :: Nat) where
  inj' :: Index n -> x a -> Symbol xs a
  prj' :: Index n -> Symbol xs a -> Maybe (x a)
instance (Denies x' xs',xs ~ (x' ': xs'),Allows' x xs' (IndexOf x xs')) => Allows' x xs ('S n) where
  inj' _ xa = Further (inj' (Index :: Index (IndexOf x xs')) xa)
  prj' _ (Further ss) = prj' (Index :: Index (IndexOf x xs')) ss
instance (Denies x xs',xs ~ (x ': xs')) => Allows' x xs 'Z where
  inj' _ = Symbol
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
    pair p (Instr ia _) (Symbol  sa) = {-# SCC "symbol_pairing" #-} pair p ia sa
    pair p (Instr _ is) (Further ss) = {-# SCC "further_pairing" #-} pair p is ss

data Plan symbols a b where
  Stop :: a -> Plan symbols a b
  Step :: Symbol symbols x -> (x -> b) -> Plan symbols a b

instance Bifunctor (Plan symbols) where
  bimap ab _ (Stop a) = Stop (ab a)
  bimap _ bc (Step symbols xb) = Step symbols (fmap bc xb)

type Uses f fs m = (Monad m,Admits' f fs (IndexOf f fs))
type Has f fs m = (Monad m,Allows' f fs (IndexOf f fs))
type Instruction f fs m = f (Instructions fs m -> m (Instructions fs m))

newtype PlanT symbols m a = PlanT { runPlanT :: m (Plan symbols a (PlanT symbols m a)) }

instance (Functor m,Monad m) => Functor (PlanT symbols m) where
  fmap f mp = {-# SCC "plan_fmap" #-}
     PlanT
     (flip liftM (runPlanT mp) $ \x -> case x of
      Stop a -> Stop (f a)
      Step symbols mb -> Step symbols (\v -> fmap f (mb v))
     )

instance Monad m => Applicative (PlanT symbols m) where
  pure a = PlanT (pure (Stop a))
  (<*>) = ap

instance (Functor m,Monad m) => Monad (PlanT symbols m) where
  return a = PlanT (pure (Stop a))
  ma >>= f = {-# SCC "plan_bind" #-}
    PlanT $ (runPlanT ma) >>= \a -> case a of
               Stop v -> runPlanT (f v)
               Step symbols xb -> pure (Step symbols (fmap (>>= f) xb))

add :: (Denies f fs) => f a -> Instrs fs a -> Instrs (f ': fs) a
add fa Empty = Instr fa Empty
add fa i = Instr fa i

(*:*) :: Denies f fs => f a -> Instrs fs a -> Instrs (f ': fs) a
(*:*) = add
infixr 5 *:*

view :: forall x xs m. Uses x xs m => Instructions xs m -> x (Instructions xs m -> m (Instructions xs m))
view xs = pull $ getInstructions xs

instruction :: forall fs x m. Uses x fs m
            => Instruction x fs m
            -> Instructions fs m -> m (Instructions fs m)
instruction x is = {-# SCC "instruction_building" #-} pure $ Instructions $ push x $ getInstructions $ is

cutoff :: Monad m => Integer -> PlanT fs m a -> PlanT fs m (Maybe a)
cutoff n _ | n <= 0 = return Nothing
cutoff n (PlanT m) = PlanT $ bimap Just (cutoff (n - 1)) `liftM` m

lift :: Monad m => m a -> PlanT symbols m a
lift ma = PlanT (fmap Stop ma)

symbol :: forall x symbols m a. (Has x symbols m) => x a -> PlanT symbols m a
symbol xa = PlanT (return (Step ({-# SCC "symbol_injection" #-} inj xa) (\x -> PlanT (return (Stop x)))))

delta :: (Pair (Instrs is) (Symbol symbols),Monad m)
       => Instructions is m
       -> PlanT symbols m r
       -> m (Instructions is m,r)
delta is (coerce -> ma) = {-# SCC "delta" #-} do
  a <- ma
  case a of
    Stop result -> pure (is,result)
    Step symbols k -> do
      (trans,nxt) <- fmap k <$> pair (curry pure) (getInstructions is) symbols
      is' <- trans is
      delta is' nxt
