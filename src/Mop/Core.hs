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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-methods       #-}
#if __GLASGOW_HASKELL__ > 710
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
module Mop.Core where

import Control.Applicative
import Control.Monad
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

data Attrs (is :: [* -> *]) a where
  Empty :: Attrs '[] a
  Attr :: Denies f fs => f a -> Attrs fs a -> Attrs (f ': fs) a

instance Functor (Attrs '[]) where
  fmap _ Empty = Empty
instance (Functor f,Functor (Attrs fs)) => Functor (Attrs (f ': fs)) where
  fmap f (Attr fa fs) = Attr (fmap f fa) (fmap f fs)

class Admits (x :: * -> *) (xs :: [* -> *]) where
  push :: x a -> Attrs xs a -> Attrs xs a
  pull :: Attrs xs a -> x a
instance (Admits' x xs (IndexOf x xs)) => Admits x xs where
  push xa = push' (Index :: Index (IndexOf x xs)) xa
  pull xs = pull' (Index :: Index (IndexOf x xs)) xs

class Admits' (x :: * -> *) (xs :: [* -> *]) (n :: Nat) where
  push' :: Index n -> x a -> Attrs xs a -> Attrs xs a
  pull' :: Index n -> Attrs xs a -> x a
instance (xs ~ (x ': xs')) => Admits' x xs 'Z where
  push' _ xa (Attr _ fs) = Attr xa fs
  pull' _ (Attr fa _) = fa
instance (Admits' x xs' (IndexOf x xs')) => Admits' x (x' ': xs') ('S n) where
  push' _ xa (Attr fa xs) = Attr fa (push' (Index :: Index (IndexOf x xs')) xa xs)
  pull' _ (Attr _ xs) = pull' (Index :: Index (IndexOf x xs')) xs

class AdmitsSubset (xs :: [* -> *]) (ys :: [* -> *]) where
  pushSubset :: Attrs xs a -> Attrs ys a -> Attrs ys a
  pullSubset :: Attrs ys a -> Attrs xs a
instance AdmitsSubset '[] ys where
  pushSubset _ ys = ys
  pullSubset _ = Empty
instance (Denies x xs,Admits' x ys (IndexOf x ys),AdmitsSubset xs ys) => AdmitsSubset (x ': xs) ys where
  pushSubset (Attr xa xs) ys = pushSubset xs (push xa ys)
  pullSubset ys = Attr (pull ys) (pullSubset ys)

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
  pair p f g = uncurry (p . f) g
instance Pair ((,) a) ((->) a) where
  pair p ~(l,r) g = p r (g l)
instance Pair (Attrs '[]) (Symbol '[])
instance (Pair i s,Pair (Attrs is) (Symbol ss))
    => Pair (Attrs (i ': is)) (Symbol (s ': ss))
  where
    pair p (Attr ia _) (Symbol  sa) = pair p ia sa
    pair p (Attr _ is) (Further ss) = pair p is ss

type family End (xs :: [k]) :: k where
  End '[x] = x
  End (x ': xs) = End xs

data Plan symbols m a
  = Pure a
  | M (m (Plan symbols m a))
  | forall b. Step (Symbol symbols b) (b -> Plan symbols m a)

{-# INLINE lift #-}
lift :: Functor m => m a -> Plan symbols m a
lift m = M (fmap Pure m)

-- | Invoke a method of the parent object. This allows:
-- > super . super $ something
{-# INLINE super #-}
super :: Functor m => Plan fs m a -> Plan gs (Plan fs m) a
super = lift

-- | Invoke a method in the calling object.
{-# INLINE self #-}
self :: Has x symbols m => x a -> Plan symbols m a
self xa = Step (inj xa) return

instance Functor m => Functor (Plan symbols m) where
  fmap f p0 = _fmap f p0

instance Monad m => Applicative (Plan symbols m) where
  pure = Pure
  (<*>) = ap

instance Monad m => Monad (Plan symbols m) where
#ifdef TRANSFORMERS_SAFE
  return = M . return . Pure
#else
  return = Pure
#endif
  (>>=) = _bind

{-# NOINLINE _fmap #-}
_fmap :: Functor m => (a -> b) -> Plan symbols m a -> Plan symbols m b
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
_bind :: Functor m => Plan symbols m a -> (a -> Plan symbols m a') -> Plan symbols m a'
p0 `_bind` f = go p0
  where
    go p =
      case p of
        Step syms k -> Step syms (\r -> go (k r))
        Pure res -> f res
        M m -> M (fmap go m)

{-# RULES
    "_bind (Step syms k) f" forall syms k f .
        _bind (Step syms k) f = Step syms (\a -> _bind (k a) f);
    "_bind (M m) f" forall m f.
        _bind (M m) f = M (fmap (flip _bind f) m);
    "_bind (Pure r) f" forall r f.
        _bind (Pure r) f = f r;
  #-}

instance MonadPlus m => Alternative (Plan fs m) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus m => MonadPlus (Plan fs m) where
  mzero = lift mzero
  mplus = _mplus

_mplus :: MonadPlus m => Plan fs m a -> Plan fs m a -> Plan fs m a
_mplus p0 p1 = go p0
  where
    go p =
      case p of
        Step sym bp -> Step sym (\b -> go (bp b))
        Pure r -> Pure r
        M m -> M (fmap go m `mplus` return p1)

instance (Monad m,Monoid r) => Monoid (Plan fs m r) where
  mempty = pure mempty
  mappend = _mappend

_mappend :: (Monad m,Monoid r) => Plan fs m r -> Plan fs m r -> Plan fs m r
_mappend p0 p1 = go p0
    where
      go p =
        case p of
          Step sym bp -> Step sym (\b -> go (bp b))
          M m -> M (fmap go m)
          Pure r -> fmap (mappend r) p1

delta :: (Pair (Attrs is) (Symbol symbols),Monad m)
       => Object is m
       -> Plan symbols m a
       -> m (Object is m,a)
delta = _delta

{-# NOINLINE _delta #-}
_delta :: forall is symbols m a. (Pair (Attrs is) (Symbol symbols),Monad m)
       => Object is m
       -> Plan symbols m a
       -> m (Object is m,a)
_delta is p0 = go p0
  where
    go :: Plan symbols m a -> m (Object is m,a)
    go p =
      case p of
        Step syms k ->
          let (trans,b) = pair (,) (objectAttrs is) syms
          in do is' <- trans is
                _delta is' (k b)
        Pure res -> pure (is,res)
        M mp -> mp >>= go

type Uses f fs m = (Monad m,Admits' f fs (IndexOf f fs))
type Extends extended orig m = (Monad m,AdmitsSubset orig extended)
type (extended :=> orig) m = Extends extended orig m

type Has f fs m = (Monad m,Allows' f fs (IndexOf f fs))
type (f :< fs) m = Has f fs m
type Invokes fs' fs m = (Monad m,AllowsSubset fs' fs)

class AllowsSubset fs' fs
instance AllowsSubset '[] fs
instance (Allows' f fs (IndexOf f fs),AllowsSubset fs' fs)
  => AllowsSubset (f ': fs') fs

type Method fs m = Object fs m -> m (Object fs m)
type Attribute f fs m = f (Method fs m)
newtype Object fs m = Object { objectAttrs :: Attrs fs (Method fs m) }

class UnsafeBuild fs where
  unsafeBuild :: Attrs fs a
instance UnsafeBuild '[] where
  unsafeBuild = Empty
instance (Typeable f,Denies f fs,UnsafeBuild fs) => UnsafeBuild (f ': fs) where
  unsafeBuild =
    let attr = show (typeOf1 (undefined :: forall a. f a))
        msg = "Attribute (" ++ attr ++ ") uninitialized."
    in Attr (error msg) unsafeBuild

{-# INLINE build #-}
build :: UnsafeBuild attrs
      => (    Attrs attrs (Method attrs m)
           -> Attrs attrs (Method attrs m)
         )
      -> Object attrs m
build f = Object $ f unsafeBuild

{-# INLINE add #-}
add :: (Denies f fs) => f a -> Attrs fs a -> Attrs (f ': fs) a
add fa Empty = Attr fa Empty
add fa i = Attr fa i

{-# INLINE (*:*)#-}
(*:*) :: Denies f fs => f a -> Attrs fs a -> Attrs (f ': fs) a
(*:*) = add
infixr 6 *:*

{-# INLINE (&) #-}
(&) :: Admits f fs => Object fs m -> Attribute f fs m
(&) xs = pull $ objectAttrs xs

infixl 5 .=
{-# INLINE (.=) #-}
(.=) :: Uses f fs m => Object fs m -> Attribute f fs m -> Object fs m
is .= x = Object $ push x $ objectAttrs is

cutoff :: Monad m => Integer -> Plan fs m a -> Plan fs m (Maybe a)
cutoff n _ | n <= 0 = return Nothing
cutoff n p =
  case p of
    Pure a -> Pure (Just a)
    M m -> M (cutoff (n - 1) `liftM` m)
    Step sym k -> Step sym (cutoff (n - 1) . k)

newtype Codensity fs m a = Codensity
  { runCodensity :: forall b. (a -> Plan fs m b) -> Plan fs m b
  }

instance Functor (Codensity fs k) where
  fmap f (Codensity m) = Codensity (\k -> m (k . f))
  {-# INLINE fmap #-}

instance Applicative (Codensity fs f) where
  pure x = Codensity (\k -> k x)
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Codensity fs f) where
  return x = Codensity (\k -> k x)
  {-# INLINE return #-}
  m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c))
  {-# INLINE (>>=) #-}

instance (Alternative v,MonadPlus v) => Alternative (Codensity fs v) where
  empty = Codensity (\_ -> empty)
  {-# INLINE empty #-}
  Codensity m <|> Codensity n = Codensity (\k -> m k <|> n k)
  {-# INLINE (<|>) #-}

instance MonadPlus v => MonadPlus (Codensity fs v) where
  mzero = Codensity (\_ -> mzero)
  {-# INLINE mzero #-}
  Codensity m `mplus` Codensity n = Codensity (\k -> m k `mplus` n k)
  {-# INLINE mplus #-}

toCodensity :: Monad m => Plan fs m a -> Codensity fs m a
toCodensity f = Codensity (f >>=)

fromCodensity :: Monad m => Codensity fs m a -> Plan fs m a
fromCodensity a = runCodensity a return

{-
-- Example usages for asymptotic improvements:

replicateM :: Monad m => Int -> Plan fs m a -> Plan fs m [a]
replicateM n f = fromCodensity $ Control.Monad.replicateM n (toCodensity f)

sequence :: Monad m => [Plan fs m a] -> Plan fs m [a]
sequence = fromCodensity . Control.Monad.sequence . map toCodensity

mapM :: Monad m => (a -> Plan fs m b) -> [a] -> Plan fs m [b]
mapM f = fromCodensity . Control.Monad.mapM (toCodensity . f)

-}

class Trans t where
  lift' :: Monad m => m a -> t m a
