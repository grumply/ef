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

class Subset (xs :: [* -> *]) (ys :: [* -> *]) where
  pushSubset :: Instrs xs a -> Instrs ys a -> Instrs ys a
  pullSubset :: Instrs ys a -> Instrs xs a
instance Subset '[] ys where
  pushSubset _ ys = ys
  pullSubset _ = Empty
instance (Denies x xs,Admits' x ys (IndexOf x ys),Subset xs ys) => Subset (x ': xs) ys where
  pushSubset (Instr xa xs) ys = pushSubset xs (push xa ys)
  pullSubset ys =
    let xa = pull ys
    in Instr xa (pullSubset ys)

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

data Plan symbols m a
  = Pure a
  | M                       (m             (Plan symbols m a))
  | forall b. Step (Symbol symbols b) (b -> Plan symbols m a)

{-# INLINE delta #-}
delta :: (Subset is is,Pair (Instrs is) (Symbol symbols),Monad m)
       => Context is m
       -> Plan symbols m a
       -> m (Context is m,a)
delta is p0 = go p0
  where
    go p =
      case p of
        Pure res -> pure (is,res)
        M mp -> do
          p' <- mp
          go p'
        Step symbols k -> do
          (trans,nxt) <- fmap k <$> pair (curry pure) is symbols
          is' <- (getTrans trans) is
          delta is' nxt

type Uses f fs m = (Monad m,Admits' f fs (IndexOf f fs))
type Has f fs m = (Monad m,Allows' f fs (IndexOf f fs))

instance (Monad m) => Functor (Plan symbols m) where
  fmap f p0 = go p0 where
    go p = case p of
      Pure a -> Pure (f a)
      M m -> M (m >>= \p' -> return (go p'))
      Step symbols fa -> Step symbols (\b -> go (fa b))

instance (Monad m) => Applicative (Plan symbols m) where
  pure = Pure
  (<*>) = ap

instance (Monad m) => Monad (Plan symbols m) where
  return = Pure
  (>>=) = _bind

{-# NOINLINE _bind #-}
_bind :: Monad m => Plan symbols m a -> (a -> Plan symbols m a') -> Plan symbols m a'
p0 `_bind` f = go p0 where
  go p = case p of
    Pure res -> f res
    M m -> M (m >>= \p' -> return (go p'))
    Step syms k -> Step syms (\r -> go (k r))

{-# RULES
    "_bind (Step syms k) f" forall syms k f .
        _bind (Step syms k) f = Step syms (\a  -> _bind (k a) f);
    "_bind (M          m) f" forall m    f .
        _bind (M          m) f = M (m >>= \p -> return (_bind p f));
    "_bind (Pure    r   ) f" forall r    f .
        _bind (Pure    r   ) f = f r;
  #-}

add :: (Denies f fs) => f a -> Instrs fs a -> Instrs (f ': fs) a
add fa Empty = Instr fa Empty
add fa i = Instr fa i

(*:*) :: Denies f fs => f a -> Instrs fs a -> Instrs (f ': fs) a
(*:*) = add
infixr 5 *:*

class UnsafeBuild fs where
  unsafeBuild :: Instrs fs a
instance UnsafeBuild '[] where
  unsafeBuild = Empty
instance (Denies f fs,UnsafeBuild fs) => UnsafeBuild (f ': fs) where
  unsafeBuild = Instr undefined unsafeBuild

build :: UnsafeBuild fs => Context fs m
build = unsafeBuild

-- An instruction table transformation, monadically.
type Transform fs m a = Instrs fs a -> m (Instrs fs a)

type Instruction f fs gs m = f (Transform gs m (Trans fs m))

newtype Trans fs m = Trans { getTrans :: Transform fs m (Trans fs m) }

type Context fs m = Instrs fs (Trans fs m)

inject :: (Subset fs gs) => Instruction f fs gs m -> Context fs m -> Context fs m
inject instr ctxt = _

-- liftTrans :: forall fs gs m.
--              (Subset fs gs,Monad m)
--           => (Instructions fs m -> m (Instructions fs m))
--           -> Instructions gs m
--           -> m (Instructions gs m)
-- liftTrans f gsI = _

cutoff :: Monad m => Integer -> Plan fs m a -> Plan fs m (Maybe a)
cutoff n _ | n <= 0 = return Nothing
cutoff n p =
  case p of
    Pure a -> Pure (Just a)
    M m -> M (cutoff (n - 1) `liftM` m)
    Step sym k -> Step sym (cutoff (n - 1) . k)

lift :: Monad m => m a -> Plan symbols m a
lift m = M (m >>= \r -> return (Pure r))

symbol :: Allows x symbols => x a -> Plan symbols m a
symbol xa = Step (inj xa) Pure

-- foldP allows recovery of interpreter at every produced value
{-# INLINE foldP #-}
foldP :: (Foldable f,Pair (Instrs is) (Symbol symbols),Monad m,Subset is is)
     => Context is m -> (a -> Plan symbols m b) -> f a -> m [(Context is m,b)]
foldP i0 ap f = foldr accumulate (const (return [])) f i0
  where
    accumulate a cont is = do
      (i,!b) <- delta is (ap a)
      ~ibs <- cont i
      return ((i,b):ibs)

-- foldP_, unlike foldP, does not allow recovery of interpreter
{-# INLINE foldP_ #-}
foldP_ :: (Foldable f,Pair (Instrs is) (Symbol symbols),Monad m,Subset is is)
     => Context is m -> (a -> Plan symbols m b) -> f a -> m [b]
foldP_ i0 ap f = foldr accumulate (const (return [])) f i0
  where
    accumulate a cont is = do
      (i,b) <- delta is (ap a)
      bs <- cont i
      return (b:bs)
