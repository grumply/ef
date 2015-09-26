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

-- import Control.Monad.Trans.Free
-- import Control.Monad.Trans.Free.Church

-- delta :: (Pair (Instrs is) (Symbol ss),Monad m)
--       => Instructions is m
--       -> FreeT (Symbol ss) m r
--       -> m (Instructions is m,r)
-- delta is ss = do
--   s <- runFreeT ss
--   let instrs = getInstructions is
--   case s of
--     Free sym -> do
--       (trans,nxt) <- pair (curry return) instrs sym
--       is' <- trans is
--       delta is' nxt
--     Pure result -> return (is,result)

-- symbol :: (MonadFree (Symbol xs) m,Allows x xs) => x a -> m a
-- symbol = liftF . inj

-- using :: (Monad m, Pair (Instrs is) (Symbol ss),Functor (Symbol ss))
--     => Instructions is m -> FT (Symbol ss) m r -> m (Instructions is m, r)
-- using is ss = delta is (fromFT ss)

-- type Has f fs m = (MonadFree (Symbol fs) m,Allows' f fs (IndexOf f fs))

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
-- instance Functor (Instrs '[]) where
--   fmap _ Empty = Empty
-- instance (Functor f,Functor (Instrs fs)) => Functor (Instrs (f ': fs)) where
--   fmap f (Instr fa fs) = Instr (fmap f fa) (fmap f fs)

add :: (Denies f fs) => f a -> Instrs fs a -> Instrs (f ': fs) a
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

(*:*) :: Denies f fs => f a -> Instrs fs a -> Instrs (f ': fs) a
(*:*) = add
infixr 5 *:*

view :: (Uses x xs m) => Instructions xs m -> x (Instructions xs m -> m (Instructions xs m))
view = pull . getInstructions

instr :: (Uses x fs m)
      => Instruction x fs m
      -> Instructions fs m -> m (Instructions fs m)
instr x = return . Instructions . push x . getInstructions

data Symbol (symbols :: [* -> *]) a where
  Symbol :: (Denies s ss) => s a -> Symbol (s ': ss) a
  Further :: Denies s ss => Symbol ss a -> Symbol (s ': ss) a
-- instance Functor (Symbol ss) where
--   fmap f (Symbol ba sb) = Symbol (f . ba) sb
--   fmap f (Further ss) = Further (fmap f ss)
class Allows x xs where
  inj :: x a -> Symbol xs a
  prj :: Functor x => Symbol xs a -> Maybe (x a)
instance (Allows' x xs (IndexOf x xs)) => Allows x xs where
  inj = inj' (Index :: Index (IndexOf x xs))
  prj = prj' (Index :: Index (IndexOf x xs))
class Allows' x xs (n :: Nat) where
  inj' :: Index n -> x a -> Symbol xs a
  prj' :: Index n -> Symbol xs a -> Maybe (x a)
instance (Denies x' xs',xs ~ (x' ': xs'),Allows' x xs' (IndexOf x xs')) => Allows' x xs ('S n) where
  inj' _ = Further . inj' (Index :: Index (IndexOf x xs'))
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
  pair p f g = uncurry (p . f) g
instance Pair ((,) a) ((->) a) where
  pair p (l,r) g = p r (g l)
instance Pair (Instrs '[]) (Symbol '[])
instance (Pair i s
         ,Pair (Instrs is) (Symbol ss)
         ) => Pair (Instrs (i ': is)) (Symbol (s ': ss)) where
  pair p (Instr ia _) (Symbol sa) = pair p ia sa
  pair p (Instr _ is) (Further ss) = pair p is ss

data Freer symbols m a where
  Purer :: a -> Freer symbols m a
  Impure :: Symbol symbols x -> (x -> m (Freer symbols m a)) -> Freer symbols m a
instance Functor m => Functor (Freer symbols m) where
  fmap f (Purer a) = Purer (f a)
  fmap f (Impure symbols k) = Impure symbols (fmap (fmap f) . k)
instance Functor m => Applicative (Freer symbols m) where
  pure = Purer
  (Purer ab) <*> (Purer a) = Purer (ab a)
  (Purer ab) <*> (Impure symbols k) = Impure symbols (fmap (fmap ab) <$> k)
  (Impure symbols mab) <*> b = Impure symbols $ fmap (<*> b) <$> mab
instance Functor m => Monad (Freer symbols m) where
  return = Purer
  Purer a >>= k = k a
  Impure symbols k' >>= k = Impure symbols (fmap (>>= k) . k')

sym :: (Allows x symbols,Monad m) => x a -> Freer symbols m a
sym xa = Impure (inj xa) (return . Purer)

delta' :: (Pair (Instrs is) (Symbol symbols),Monad m)
       => Instructions is m
       -> Freer symbols m r
       -> m (Instructions is m,r)
delta' is fs =
  let instrs = getInstructions is
  in case fs of
       Purer result -> return (is,result)
       Impure symbols k -> do
         (trans,nxt) <- pair (curry return) instrs symbols
         is' <- trans is
         nxt' <- k nxt
         delta' is' nxt'


build = Instructions
empty = Empty
single = (*:* empty)
simple = build . single

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

type Uses f fs m = (Admits' f fs (IndexOf f fs),Monad m)
type Has f fs m = (Functor m,Allows' f fs (IndexOf f fs))
type Instruction f fs m = f (Instructions fs m -> m (Instructions fs m))

data State st k  = Get    (st -> k) | Put (st  , k)
data Store st k  = Store  (st  , k)       (st -> k)

get :: (Monad m,Allows (State st) fs) => Freer fs m st
get = sym (Get id)

put :: (Monad m,Allows (State st) fs) => st -> Freer fs m ()
put st = sym (Put (st,()))

modify :: (Monad m,Allows (State st) fs) => (st -> st) -> Freer fs m ()
modify f = get >>= (put . f)

store :: Uses (Store st) fs m => st -> Instruction (Store st) fs m
store st = Store (st,return) (instr . store)

instance Pair (Store st) (State st) where
  pair p (Store pos _) (Get stk)  = pair p pos stk
  pair p (Store _ seek) (Put stk) = pair p seek stk

newtype I = I Int
succI (I n) = I (succ n)

main = do
  let st = simple $ store (I 0)
  (c,ci) <- delta' st $ do
                I x <- get
                return (x + 1)
  print ci
