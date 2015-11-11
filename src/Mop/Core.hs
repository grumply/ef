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
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-methods       #-}
#if __GLASGOW_HASKELL__ > 710
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
module Mop.Core where


import Control.Applicative
import Control.Monad
import Data.Typeable hiding (cast)
import Unsafe.Coerce
import Debug.Trace

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> :set -XNoMonomorphismRestriction
-- >>> :set -XFlexibleContexts
-- >>> :set -XDataKinds

-- Insert a into xs treating xs as a set
type family (:+:) a xs where
  a :+: '[] = '[a]
  a :+: (a ': xs) = (a ': xs)
  a :+: (x ': xs) = x ': a :+: xs

-- Union xs with ys
-- for emacs: c-x 8 <enter> 222a <enter>
type family (∪) xs ys where
  '[] ∪ ys = ys
  (x ': xs) ∪ ys = x :+: (xs ∪ ys)

type family In (x :: k) (xs :: [k]) :: Bool where
  In x '[] = 'False
  In x (x ': xs) = 'True
  In x (y ': xs) = In x xs

class Subset xs ys
instance Subset '[] ys
instance (In x ys ~ 'True,Subset xs ys) => Subset (x ': xs) ys

type family (/==) (x :: k) (y :: k) :: Bool where
  (/==) x x = 'False
  (/==) x y = 'True

class Denies (x :: k) (ys :: [k])
instance Denies x '[]
instance ((x /== y) ~ 'True,Denies x ys) => Denies x (y ': ys)

data Nat = Z | S Nat
data Index (n :: Nat)= Index
type family IndexOf (f :: k) (fs :: [k]) :: Nat where
  IndexOf f (f ': fs) = 'Z
  IndexOf f (any ': fs) = 'S (IndexOf f fs)

data Attrs (as :: [* -> *]) a where
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

stretch :: (Admits' x xs (IndexOf x xs))
        => (forall z. x z -> x z)
        -> Attrs xs a
        -> Attrs xs a
stretch f xs = push (f $ pull xs) xs

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

data Symbol symbols a where
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
  prj' _ _ = Nothing
instance (Denies x xs',xs ~ (x ': xs')) => Allows' x xs 'Z where
  inj' _ = Symbol
  prj' _ (Symbol sa) = Just sa
  prj' _ (Further _) = Nothing

-- Witness a symmetry between f and g with a specific use case
class Symmetry f g | f -> g, g -> f where
  symmetry :: (a -> b -> r) -> f a -> g b -> r
instance Symmetry ((->) a) ((,) a) where
  symmetry use f g = uncurry (use . f) g
instance Symmetry ((,) a) ((->) a) where
  symmetry use ~(l,r) g = use r (g l)
instance Symmetry (Attrs '[]) (Symbol '[])
instance (Symmetry i s,Symmetry (Attrs is) (Symbol ss))
    => Symmetry (Attrs (i ': is)) (Symbol (s ': ss))
  where
    symmetry use (Attr ia _) (Symbol  sa) = symmetry use ia sa
    symmetry use (Attr _ is) (Further ss) = symmetry use is ss

cast :: forall fs gs m a. (Functor m,As (Symbol fs) (Symbol gs)) => Pattern fs m a -> Pattern gs m a
cast (Step sym bp) = Step (conv sym) (unsafeCoerce bp)
cast (M m) = M (fmap cast m)
cast (Pure r) = Pure r

rearrange :: (Functor m, Allows' (Symbol s) s' (IndexOf (Symbol s) s'))
       => Pattern s m a -> Pattern s' m a
rearrange (Step sym bp) = Step (inj sym) (unsafeCoerce bp)
rearrange (M m) = M (fmap rearrange m)
rearrange (Pure r) = Pure r

class As x y where
  conv :: x a -> y a
instance As x x where
  conv = id
instance As (Symbol '[]) (Symbol '[])
instance {-# OVERLAPPABLE #-} (As x y,As (Symbol xs) (Symbol ys),Denies y ys)
  => As (Symbol (x ': xs)) (Symbol (y ': ys)) where
  conv (Symbol sa) = Symbol (conv sa)
  conv (Further ss) = Further (conv ss)

type family End (xs :: [k]) :: k where
  End '[x] = x
  End (x ': xs) = End xs

data Pattern symbols m a
  = forall b. Step (Symbol symbols b) (b -> Pattern symbols m a)
  | M (m (Pattern symbols m a))
  | Pure a

class Functor m' => Lift m m' where
  lift :: m a -> m' a
instance Functor m => Lift m (Pattern fs m) where
  lift = lift_
instance Lift m m' => Lift m (Pattern fs m') where
  lift = lift_ . lift

lift_ :: Functor m => m a -> Pattern symbols m a
lift_ m = M (fmap Pure m)

-- | Invoke a method of the parent object. This allows:
-- > super . super $ something
super :: Functor m => Pattern fs m a -> Pattern gs (Pattern fs m) a
super = lift

-- | Invoke a method in the calling object.
self :: (Is x symbols m) => x a -> Pattern symbols m a
self xa = Step (inj xa) return

instance Functor m => Functor (Pattern symbols m) where
  fmap f p0 = _fmap f p0

instance Monad m => Applicative (Pattern symbols m) where
  pure = Pure
  (<*>) = ap
  (*>) = (>>)

instance Monad m => Monad (Pattern symbols m) where
#ifdef TRANSFORMERS_SAFE
  return = M . return . Pure
#else
  return = Pure
#endif
  (>>=) = _bind

{-# NOINLINE _fmap #-}
_fmap :: Functor m => (a -> b) -> Pattern symbols m a -> Pattern symbols m b
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
_bind :: Functor m => Pattern symbols m a -> (a -> Pattern symbols m a') -> Pattern symbols m a'
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

instance MonadPlus m => Alternative (Pattern fs m) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus m => MonadPlus (Pattern fs m) where
  mzero = lift_ mzero
  mplus = _mplus

_mplus :: MonadPlus m => Pattern fs m a -> Pattern fs m a -> Pattern fs m a
_mplus p0 p1 = go p0
  where
    go p =
      case p of
        Step sym bp -> Step sym (\b -> go (bp b))
        Pure r -> Pure r
        M m -> M (fmap go m `mplus` return p1)

instance (Monad m,Monoid r) => Monoid (Pattern fs m r) where
  mempty = pure mempty
  mappend = _mappend

_mappend :: (Monad m,Monoid r) => Pattern fs m r -> Pattern fs m r -> Pattern fs m r
_mappend p0 p1 = go p0
    where
      go p =
        case p of
          Step sym bp -> Step sym (\b -> go (bp b))
          M m -> M (fmap go m)
          Pure r -> fmap (mappend r) p1

observe :: forall fs m a. Monad m => Pattern fs m a -> Pattern fs m a
observe = M . go
  where
    go p =
      case p of
        Step sym bp -> return (Step sym (\b -> observe (bp b)))
        M m -> m >>= go
        Pure r -> return (Pure r)

(#) :: (Symmetry (Attrs is) (Symbol symbols),Monad m)
    => m (Object is m)
    -> Pattern symbols m a
    -> m (Object is m)
(#) mobj p = do
  obj <- mobj
  (obj',_) <- delta obj p
  return obj'

deltaCast :: forall fs gs is m a. (Symmetry (Attrs is) (Symbol gs),Monad m,As (Symbol fs) (Symbol gs))
          => Object is m
          -> Pattern fs m a
          -> m (Object is m,a)
deltaCast o = _delta o . (cast :: Pattern fs m a -> Pattern gs m a)

run :: Monad m => Pattern '[] m a -> m a
run = fmap snd . delta simple

delta :: forall symbols is m a. (Symmetry (Attrs is) (Symbol symbols),Monad m)
       => Object is m
       -> Pattern symbols m a
       -> m (Object is m,a)
delta = _delta

{-# NOINLINE _delta #-}
_delta :: forall is symbols m a. (Symmetry (Attrs is) (Symbol symbols),Monad m)
       => Object is m
       -> Pattern symbols m a
       -> m (Object is m,a)
_delta = go
  where
    go is = go'
      where
        go' p =
          case p of
            Step syms k ->
              let ~(trans,b) = symmetry (,) (deconstruct is) syms
              in do is' <- trans is
                    go is' (k b)
            M mp -> mp >>= go'
            Pure res -> pure (is,res)

deltaDebug :: forall is symbols m a. (Symmetry (Attrs is) (Symbol symbols),Monad m,Typeable symbols,Typeable is)
           => Object is m
           -> Pattern symbols m a
           -> m (Object is m,(Int,a))
deltaDebug = _deltaDebug

{-# NOINLINE _deltaDebug #-}
_deltaDebug :: forall is symbols m a. (Symmetry (Attrs is) (Symbol symbols),Monad m,Typeable symbols,Typeable is)
            => Object is m
            -> Pattern symbols m a
            -> m (Object is m,(Int,a))
_deltaDebug = go 0
  where
    go n is = go'
      where
        go' p =
          case p of
            Step syms k ->
              let ~(trans,b) = symmetry (,) (deconstruct is) syms
              in do is' <- trans is
                    let n' = n + 1
                    trace (show (typeOf (unsafeCoerce syms :: Symbol symbols ()))) $ n' `seq` go n' is' (k b)
            M m -> m >>= go'
            Pure r -> pure (is,(n,r))


type Uses f fs m = (Monad m,Admits' f fs (IndexOf f fs))
type Extends extended orig m = (Monad m,AdmitsSubset orig extended)
type (extended :=> orig) m = Extends extended orig m

type Is f fs m = (Monad m,Allows' f fs (IndexOf f fs))
type (f :< fs) m = Is f fs m
type Invokes fs' fs m = (Monad m,AllowsSubset fs' fs)

class AllowsSubset fs' fs
instance AllowsSubset '[] fs
instance (Allows' f fs (IndexOf f fs),AllowsSubset fs' fs)
  => AllowsSubset (f ': fs') fs

type Method fs m = Object fs m -> m (Object fs m)
type Attribute f fs m = f (Method fs m)
newtype Object fs m = Object { deconstruct :: Attrs fs (Method fs m) }

simple :: Monad m => Object '[] m
simple = Object Empty

class UnsafeBuild fs where
  unsafeBuild :: Attrs fs a
instance UnsafeBuild '[] where
  unsafeBuild = Empty
instance (Typeable f,Denies f fs,UnsafeBuild fs) => UnsafeBuild (f ': fs) where
  unsafeBuild =
    let attr = show (typeOf1 (undefined :: forall a. f a))
        msg = "Attribute (" ++ attr ++ ") uninitialized."
    in Attr (error msg) unsafeBuild

build :: UnsafeBuild attrs
      => (    Attrs attrs (Method attrs m)
           -> Attrs attrs (Method attrs m)
         )
      -> Object attrs m
build f = Object $ f unsafeBuild

(*:*) :: Denies f fs => f a -> Attrs fs a -> Attrs (f ': fs) a
(*:*) fa Empty = Attr fa Empty
(*:*) fa i = Attr fa i
infixr 6 *:*

view :: Admits f fs => Object fs m -> Attribute f fs m
view xs = pull $ deconstruct xs

infixl 5 .=
(.=) :: Uses f fs m => Object fs m -> Attribute f fs m -> Object fs m
is .= x = Object $ push x $ deconstruct is

-- | cutoffSteps limits the number of Step constructors in a 'Pattern'. To limit
-- the number of (Step constructors + M constructors), use 'cutoff'.
--
-- >>> import Mop.Core
-- >>> import Effect.State
-- >>> newtype St = St Int
-- >>> :{
--  let inc (St n) = St (n + 1)
--  in do (o,_) <- delta (Object $ store (St 0) *:* Empty) $
--                   cutoffSteps 3 $ replicateM_ 5 (modify inc)
--        (_,St i) <- delta o get
--        print i
-- :}
--3
cutoffSteps :: Monad m => Integer -> Pattern fs m a -> Pattern fs m (Maybe a)
cutoffSteps n _ | n <= 0 = return Nothing
cutoffSteps n p =
  case p of
    Pure a -> Pure (Just a)
    M m -> M (cutoff (n - 1) `liftM` m)
    Step sym k -> Step sym (cutoff (n - 1) . k)

cutoff :: Monad m => Integer -> Pattern fs m a -> Pattern fs m (Maybe a)
cutoff n _ | n <= 0 = return Nothing
cutoff n p =
  case p of
    Pure a -> Pure (Just a)
    M m -> M (cutoff (n - 1) `liftM` m)
    Step sym k -> Step sym (cutoff (n - 1) . k)

--------------------------------------------------------------------------------
-- | Codensity improves asymptotics of repeated left-associated binds by
--   conversion to right-associated binds.
--
-- This example demonstrates a performance improvement over the standard
-- replicateM.
--
-- >>> import Data.Time
-- >>> import System.Timeout
-- >>> :{
--   let
--     time f = do { s <- getCurrentTime; r <- f; e <- r `seq` getCurrentTime
--                 ; return (diffUTCTime e s,r) }
--     replicateP n = fromCodensity . Control.Monad.replicateM n . toCodensity
--   in do (conventionalTime,res0) <- time (run (replicateM 10 (return ())))
--         (codensityTime,res1)    <- time (run (replicateP 10 (return ())))
--         return (conventionalTime - codensityTime > 0)
-- :}
--True

newtype Codensity fs m a = Codensity
  { runCodensity :: forall b. (a -> Pattern fs m b) -> Pattern fs m b
  }

instance Functor (Codensity fs k) where
  fmap f (Codensity m) = Codensity (\k -> m (k . f))

instance Applicative (Codensity fs f) where
  pure x = Codensity (\k -> k x)
  (<*>) = ap

instance Monad (Codensity fs f) where
  return x = Codensity (\k -> k x)
  m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c))

instance (Alternative v,MonadPlus v) => Alternative (Codensity fs v) where
  empty = Codensity (\_ -> empty)
  Codensity m <|> Codensity n = Codensity (\k -> m k <|> n k)

instance MonadPlus v => MonadPlus (Codensity fs v) where
  mzero = Codensity (\_ -> mzero)
  Codensity m `mplus` Codensity n = Codensity (\k -> m k `mplus` n k)

toCodensity :: Monad m => Pattern fs m a -> Codensity fs m a
toCodensity f = Codensity (f >>=)

fromCodensity :: Monad m => Codensity fs m a -> Pattern fs m a
fromCodensity a = runCodensity a return
