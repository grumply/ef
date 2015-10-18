{-
Leaving these pragmas here reference.
Pragmas for other modules are in the cabal file.
-}
{-# LANGUAGE PolyKinds                          #-}
{-# LANGUAGE ConstraintKinds                    #-}
{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE FlexibleContexts                   #-}
{-# LANGUAGE FlexibleInstances                  #-}
{-# LANGUAGE FunctionalDependencies             #-}
{-# LANGUAGE KindSignatures                     #-}
{-# LANGUAGE MultiParamTypeClasses              #-}
{-# LANGUAGE RankNTypes                         #-}
{-# LANGUAGE ScopedTypeVariables                #-}
{-# LANGUAGE TypeFamilies                       #-}
{-# LANGUAGE TypeOperators                      #-}
{-# LANGUAGE GADTs                              #-}
{-# LANGUAGE UndecidableInstances               #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-missing-methods       #-}
#if __GLASGOW_HASKELL__ > 710
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
module Mop where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.Typeable

type family (:++:) (xs :: [k]) (ys :: [k]) :: [k] where
  xs        :++: '[] = xs
  '[]       :++: ys  = ys
  (x ': xs) :++: ys  = x ': (xs :++: ys)

type family In (x :: k) (xs :: [k]) :: Bool where
  In x '[] = 'False
  In x (x ': xs) = 'True
  In x (y ': xs) = In x xs
class Subset xs ys
instance Subset '[] ys
instance (In x ys ~ 'True,Subset xs ys) => Subset (x ': xs) ys

type family NotEq (x :: k) (y :: k) :: Bool where
  NotEq x x = 'False
  NotEq x y = 'True
class Denies (x :: k) (ys :: [k])
instance Denies x '[]
instance (NotEq x y ~ 'True,Denies x ys) => Denies x (y ': ys)

data Nat = Z | S Nat
data Index (n :: Nat)= Index
type family IndexOf (f :: k) (fs :: [k]) :: Nat where
  IndexOf f (f ': fs) = 'Z
  IndexOf f (any ': fs) = 'S (IndexOf f fs)

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
instance (Admits' x xs (IndexOf x xs)) => Admits x xs where
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

class AdmitsSubset (xs :: [* -> *]) (ys :: [* -> *]) where
  pushSubset :: Instrs xs a -> Instrs ys a -> Instrs ys a
  pullSubset :: Instrs ys a -> Instrs xs a
instance AdmitsSubset '[] ys where
  pushSubset _ ys = ys
  pullSubset _ = Empty
instance (Denies x xs,Admits' x ys (IndexOf x ys),AdmitsSubset xs ys) => AdmitsSubset (x ': xs) ys where
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
  {-# INLINE inj' #-}
  inj' _ xa = Further (inj' (Index :: Index (IndexOf x xs')) xa)
  {-# INLINE prj' #-}
  prj' _ (Further ss) = prj' (Index :: Index (IndexOf x xs')) ss
  prj' _ _ = Nothing
instance (Denies x xs',xs ~ (x ': xs')) => Allows' x xs 'Z where
  {-# INLINE inj' #-}
  inj' _ = Symbol
  {-# INLINE prj' #-}
  prj' _ (Symbol sa) = Just sa
  prj' _ (Further _) = Nothing

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
    pair p (Instr ia _) (Symbol  sa) = pair p ia sa
    pair p (Instr _ is) (Further ss) = pair p is ss

type family End (xs :: [k]) :: k where
  End '[x] = x
  End (x ': xs) = End xs

data PlanT symbols m a
  = Pure a
  | M (m (PlanT symbols m a))
  | forall b. Step (Symbol symbols b) (b -> PlanT symbols m a)

type Plan symbols a = PlanT symbols Identity a

{-# INLINE lift #-}
lift :: Functor m => m a -> PlanT symbols m a
lift m = M (fmap Pure m)

{-# INLINE symbol #-}
symbol :: Allows x symbols => x a -> PlanT symbols m a
symbol xa = Step (inj xa) Pure

instance Functor m => Functor (PlanT symbols m) where
  fmap f p0 = _fmap f p0

instance Functor m => Applicative (PlanT symbols m) where
  pure = Pure
  (<*>) = ap

instance Functor m => Monad (PlanT symbols m) where
#ifdef TRANSFORMERS_SAFE
  return = M . fmap Pure
#else
  return = Pure
#endif
  (>>=) = _bind

{-# NOINLINE _fmap #-}
_fmap :: Functor m => (a -> b) -> PlanT symbols m a -> PlanT symbols m b
_fmap f p0 = go p0
  where
    go p =
      case p of
        Pure a -> Pure (f a)
        M m -> M (fmap go m)
        Step syms bp -> Step syms (\b -> go (bp b))

{-# RULES
  "_fmap f (Step syms k)" forall syms k f.
      _fmap f (Step syms k) = Step syms (\a -> _fmap f (k a));
  "_fmap f (M m)" forall f m.
      _fmap f (M m) = M (fmap (_fmap f) m);
  "_fmap f (Pure r)" forall f r.
      _fmap f (Pure r) = Pure (f r);
  #-}

{-# NOINLINE _bind #-}
_bind :: Functor m => PlanT symbols m a -> (a -> PlanT symbols m a') -> PlanT symbols m a'
p0 `_bind` f = go p0
  where
    go p =
      case p of
        Pure res -> f res
        M m -> M (fmap go m)
        Step syms k -> Step syms (\r -> go (k r))

{-# RULES
    "_bind (Step syms k) f" forall syms k f .
        _bind (Step syms k) f = Step syms (\a -> _bind (k a) f);
    "_bind (M m) f" forall m f.
        _bind (M m) f = M (fmap (flip _bind f) m);
    "_bind (Pure r) f" forall r f.
        _bind (Pure r) f = f r;
  #-}

instance MonadPlus m => Alternative (PlanT fs m) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus m => MonadPlus (PlanT fs m) where
  mzero = lift mzero
  mplus = _mplus

_mplus :: MonadPlus m => PlanT fs m a -> PlanT fs m a -> PlanT fs m a
_mplus p0 p1 = go p0
  where
    go p =
      case p of
        Step sym bp -> Step sym (\b -> go (bp b))
        Pure r -> Pure r
        M m -> M (fmap go m `mplus` return p1)

instance (Monad m,Monoid r) => Monoid (PlanT fs m r) where
  mempty = pure mempty
  mappend = _mappend

_mappend :: (Monad m,Monoid r) => PlanT fs m r -> PlanT fs m r -> PlanT fs m r
_mappend p0 p1 = go p0
    where
      go p =
        case p of
          Step sym bp -> Step sym (\b -> go (bp b))
          M m -> M (fmap go m)
          Pure r -> fmap (mappend r) p1

delta :: (Pair (Instrs is) (Symbol symbols),Monad m)
       => InstructionsT is m
       -> PlanT symbols m a
       -> m (InstructionsT is m,a)
delta = _delta

{-# NOINLINE _delta #-}
_delta :: forall is symbols m a. (Pair (Instrs is) (Symbol symbols),Monad m)
       => InstructionsT is m
       -> PlanT symbols m a
       -> m (InstructionsT is m,a)
_delta is p0 = go p0
  where
    go :: PlanT symbols m a -> m (InstructionsT is m,a)
    go p =
      case p of
        Pure res -> pure (is,res)
        M mp -> mp >>= go
        Step syms k ->
          let (trans,b) = pair (\a x -> (a,x)) (getContext is) syms
          in trans is >>= \is' -> _delta is' (k b)

type Uses f fs m = (Monad m,Admits' f fs (IndexOf f fs))
type Using fs' fs m = (Monad m,AdmitsSubset fs' fs)
type Has f fs m = (Monad m,Allows' f fs (IndexOf f fs))
type Having fs' fs m = (Monad m,AllowsSubset fs' fs)

class AllowsSubset fs' fs
instance AllowsSubset '[] fs
instance (Allows' f fs (IndexOf f fs),AllowsSubset fs' fs)
  => AllowsSubset (f ': fs') fs

type TransformationT instrs m
  =      (InstructionsT instrs m)
    -> m (InstructionsT instrs m)

type Transformation instrs = TransformationT instrs Identity

type Instruction instr instrs m = instr (TransformationT instrs m)
type Context instrs m = Instrs instrs (TransformationT instrs m)
type Build instrs m = Context instrs m -> Context instrs m
newtype InstructionsT instrs m = Instructions { getContext :: Context instrs m }
type Instructions instrs = InstructionsT instrs Identity

class UnsafeBuild fs where
  unsafeBuild :: Instrs fs a
instance UnsafeBuild '[] where
  unsafeBuild = Empty
instance (Typeable f,Denies f fs,UnsafeBuild fs) => UnsafeBuild (f ': fs) where
  unsafeBuild =
    let instr = show (typeOf1 (undefined :: forall a. f a))
        msg = "Instruction (" ++ instr ++ ") uninitialized."
    in Instr (error msg) unsafeBuild

{-# INLINE build #-}
build :: UnsafeBuild instrs => Build instrs m -> InstructionsT instrs m
build f = Instructions $ f unsafeBuild

class UnsafeBuild' fs where
  unsafeBuild' :: Instrs fs a
instance UnsafeBuild' '[] where
  unsafeBuild' = Empty
instance (Denies f fs,UnsafeBuild' fs) => UnsafeBuild' (f ': fs) where
  unsafeBuild' = Instr undefined unsafeBuild'

build' :: UnsafeBuild' instrs => Build instrs m -> InstructionsT instrs m
build' f = Instructions $ f unsafeBuild'

{-# INLINE add #-}
add :: (Denies f fs) => f a -> Instrs fs a -> Instrs (f ': fs) a
add fa Empty = Instr fa Empty
add fa i = Instr fa i

{-# INLINE (*:*)#-}
(*:*) :: Denies f fs => f a -> Instrs fs a -> Instrs (f ': fs) a
(*:*) = add
infixr 5 *:*

{-# INLINE view #-}
view :: Admits instr instrs => InstructionsT instrs m -> Instruction instr instrs m
view xs = pull $ getContext xs

{-# INLINE instruction #-}
instruction :: Uses instr instrs m => Instruction instr instrs m -> InstructionsT instrs m -> m (InstructionsT instrs m)
instruction x is = {-# SCC "instruction_building" #-} pure $ Instructions $ push x $ getContext is

cutoff :: Monad m => Integer -> PlanT fs m a -> PlanT fs m (Maybe a)
cutoff n _ | n <= 0 = return Nothing
cutoff n p =
  case p of
    Pure a -> Pure (Just a)
    M m -> M (cutoff (n - 1) `liftM` m)
    Step sym k -> Step sym (cutoff (n - 1) . k)
