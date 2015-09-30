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

import Control.Monad
import Control.Monad.Fix

import Data.Bifunctor

import Data.Coerce

--------------------------------------------------------------------------------
-- Strict state

data State st k
  = Get (st -> k)
  | Put st k
get :: Has (State st) fs m => PlanT fs m st
get = sym (Get id)
put :: (Has (State st) fs m) => st -> PlanT fs m ()
put st = sym (Put st ())

data Store st k = Store st k (st -> k)
store :: Uses (Store st) fs m => st -> Instruction (Store st) fs m
store st = Store st pure (instr . store)

instance Pair (Store st) (State st) where
  pair p (Store st k _  ) (Get stk ) = pair p (st,k) stk
  pair p (Store _  _ stk) (Put st k) = pair p stk (st,k)

modify :: Has (State st) fs m => (st -> st) -> PlanT fs m ()
modify f = do
  st <- get
  put $ f st

modify' :: Has (State st) fs m => (st -> st) -> PlanT fs m ()
modify' f = do
  st <- get
  put $! f st

lazy :: Has (State Integer) fs m => PlanT fs m [Integer]
lazy = mapM (\n -> modify' (n+) >> get) [1::Integer .. 5]

main = do
  (_,x :: Integer) <- delta (build $ store (0 :: Integer) *:* empty) $ (cutoff 10 lazy >> get)
  print x
  return ()

cutoff :: Monad m => Integer -> PlanT fs m a -> PlanT fs m (Maybe a)
cutoff n _ | n <= 0 = return Nothing
cutoff n (PlanT m) = PlanT $ bimap Just (cutoff (n - 1)) `liftM` m

--------------------------------------------------------------------------------
-- Implementation

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
  {-# INLINE pair #-}
  pair p f g = uncurry (\x -> p (f x)) g
instance Pair ((,) a) ((->) a) where
  {-# INLINE pair #-}
  pair p (l,r) g = p r (g l)
instance Pair (Instrs '[]) (Symbol '[])
instance (Pair i s,Pair (Instrs is) (Symbol ss))
    => Pair (Instrs (i ': is)) (Symbol (s ': ss))
  where
    {-# INLINE pair #-}
    pair p (Instr ia _) (Symbol  sa) = pair p ia sa
    pair p (Instr _ is) (Further ss) = pair p is ss

data Plan symbols a b where
  Stop :: a -> Plan symbols a b
  Step :: Symbol symbols x -> (x -> b) -> Plan symbols a b

instance Bifunctor (Plan symbols) where
  bimap ab _ (Stop a) = Stop (ab a)
  bimap _ bc (Step symbols xb) = Step symbols (fmap bc xb)

newtype PlanT symbols m a = PlanT { runPlanT :: m (Plan symbols a (PlanT symbols m a)) }

coerceToPlanT :: m (Plan symbols a (PlanT symbols m a)) -> PlanT symbols m a
coerceToPlanT = coerce

coerceFromPlanT :: PlanT symbols m a -> m (Plan symbols a (PlanT symbols m a))
coerceFromPlanT = coerce

instance (Functor m,Monad m) => Functor (PlanT symbols m) where
  {-# INLINE fmap #-}
  fmap f (coerceFromPlanT -> mp) =
    coerceToPlanT (flip liftM mp $ \x -> case x of
      Stop a -> Stop (f a)
      Step symbols mb -> Step symbols (\v -> fmap f (mb v))
     )

instance Monad m => Applicative (PlanT symbols m) where
  {-# INLINE pure #-}
  pure a = coerceToPlanT (pure (Stop a))
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance (Functor m,Monad m) => Monad (PlanT symbols m) where
  {-# INLINE return #-}
  return a = coerceToPlanT (pure (Stop a))
  {-# INLINE (>>=) #-}
  (coerce -> ma) >>= f = coerce $ ma >>= \a -> case a of
    Stop v -> coerceFromPlanT (f v)
    Step symbols xb -> pure (Step symbols (fmap (>>= f) xb))

add :: (Denies f fs) => f a -> Instrs fs a -> Instrs (f ': fs) a
add fa Empty = Instr fa Empty
add fa i = Instr fa i

observe :: Instrs (f ': fs) a -> (f a,Instrs fs a)
observe (Instr fa fs) = (fa,fs)

(*:*) :: Denies f fs => f a -> Instrs fs a -> Instrs (f ': fs) a
(*:*) = add
infixr 5 *:*

build :: Instrs fs (Instructions fs m -> m (Instructions fs m)) -> Instructions fs m
build = coerce

unbuild :: Instructions fs m -> Instrs fs (Instructions fs m -> m (Instructions fs m))
unbuild = coerce

empty :: Instrs '[] a
empty = Empty

single :: f a -> Instrs '[f] a
single = (*:* empty)

simple :: f (Instructions '[f] m -> m (Instructions '[f] m)) -> Instructions '[f] m
simple t = build $ single t

view :: Uses x xs m => Instructions xs m -> x (Instructions xs m -> m (Instructions xs m))
view xs = pull $ unbuild xs

instr :: Uses x fs m
      => Instruction x fs m
      -> Instructions fs m -> m (Instructions fs m)
instr x is = pure $ build $ push x $ unbuild is

lift :: Monad m => m a -> PlanT symbols m a
lift ma = coerceToPlanT (fmap Stop ma)

sym :: (Has x symbols m) => x a -> PlanT symbols m a
sym xa = coerceToPlanT $ pure $ Step (inj xa) (coerceToPlanT . pure . Stop)

delta :: (Pair (Instrs is) (Symbol symbols),Monad m)
       => Instructions is m
       -> PlanT symbols m r
       -> m (Instructions is m,r)
delta is (coerceFromPlanT -> ma) = do
  a <- ma
  case a of
    Stop result -> pure (is,result)
    Step symbols k -> do
      (trans,nxt) <- fmap k <$> pair (curry pure) (unbuild is) symbols
      is' <- trans is
      delta is' nxt

type Uses f fs m = (Monad m,Admits' f fs (IndexOf f fs))
type Has f fs m = (Monad m,Allows' f fs (IndexOf f fs))

type Instruction f fs m = f (Instructions fs m -> m (Instructions fs m))

data CC k' k
  = Checkpoint (k' -> k)
  | Recall k' k
data Reify k' k = Reify (k',k) (k' -> k)

checkpoint :: Monad m => PlanT '[CC (Cont m a)] m (Cont m a)
checkpoint = sym (Checkpoint id)

recall :: Monad m => Cont m a -> PlanT '[CC (Cont m a)] m (Cont m a)
recall plan = sym (Recall plan plan)

instance Pair (Reify k) (CC k) where
  pair p (Reify kl _) (Checkpoint k) = pair p kl k
  pair p (Reify _ kr) (Recall k' k) = pair p kr (k',k)

reify :: Uses (Reify (Cont m a)) symbols m => Cont m a -> Instruction (Reify (Cont m a)) symbols m
reify f = Reify (f,pure) (instr . reify)

-- x :: (Has (State [Int]) fs IO,Has (CC (PlanT fs IO [Int])) fs IO) => PlanT fs IO [Int]
-- x = do
--   let one = 1 :: Int
--   checkpoint
--   ln <- lift getLine
--   modify (one:)
--   st :: [Int] <- get
--   lift (putStrLn $ "Current: " ++ show st)
--   case ln of
--     "y" -> x
--     "n" -> do
--       recall
--       get

newtype Cont m a = Cont { getCont :: PlanT '[CC (Cont m a)] m a }

-- cont :: Cont m a -> m (Instructions '[Reify (Cont m a)] m,a)
-- cont c = delta (build $ reify c *:* empty) (getCont c)

-- main :: IO ()
-- main = do
--   -- (c,i :: Int) <- delta (build $ store (0 :: Int) *:* empty) countdown
--   -- print i
--   (_,ln) <- cont $ Cont $ do
--               p <- checkpoint
--               ln <- lift getLine
--               lift (putStrLn ln)
--               case ln of
--                 "y" -> _ $ recall p
--                 "n" -> return ln
--   print ln
--   return ()
