{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}
{-
Product and Pairing were largely the work of Dave Laing and his cofun
series on github at https://github.com/dalaing/cofun and Swierstra's
Data Types a la Carte. Matthew Pickering's mpickering.github.io blog
had a wonderful post about a weaker version of compdata's subsumption/
dependency injection type families that was largely integrated.
-}

module Mop (module Export,showFT,showF) where

import Control.Monad.Fix

import Control.Monad as Export
import Control.Comonad as Export

import Control.Monad.Trans.Free as Export
import Control.Comonad.Trans.Cofree as Export

import Control.Comonad.Store    as Export
import Control.Comonad.Env      as Export
import Control.Comonad.Identity as Export
import Control.Comonad.Traced   as Export hiding (Sum(..),Product(..))

import qualified Control.Monad.Trans as Trans

import Sum         as Export
import Product     as Export
import Subsumption as Export
import Pairing     as Export
import Optimize    as Export
import Synonyms    as Export
import Generate    as Export (expand,mop,Verbosity(..))
import Checked     as Export

import Control.Monad.Catch as Export

import qualified Control.Monad.Free as Free
import qualified Control.Comonad.Cofree as Cofree

import Language.Haskell.TH.Syntax

instance (Lift (f b),Lift a) => Lift (FreeF f a b) where
  lift (Pure x) = [| Pure x |]
  lift (Free fb) = [| Free fb |]

instance (Lift (m (FreeF f a (FreeT f m a)))) => Lift (FreeT f m a) where
  lift (FreeT f) = [| FreeT f |]

instance Lift a => Lift (Identity a) where
  lift (Identity a) = [| Identity a |]

showFT :: (Show (f a),Show a,Show (f (FreeT f Identity a))) => FreeT f Identity a -> String
showFT f = show $ runIdentity $ runFreeT f

showF :: (Show (f b),Show a) => FreeF f a b -> String
showF (Free fb) = show fb
showF (Pure a) = show a

-- because why not
class Comonad w => ComonadCofix w where
  wfix :: (w a -> a) -> w a
  wfix = cfix

instance (MonadFix m,Functor f,Algebra f m)
  => MonadFix (FreeT f m)
  where
    mfix = Trans.lift . mfix . (run .)
    -- this monadfix instance is the reason for the following classes

class (Monad m,Functor f) => Algebra f m where
  wrap :: f (m a) -> m a
  run :: FreeT f m a -> m a
  run = iterT Mop.wrap
instance (Functor f,MonadFree f m) => Algebra f m where
  wrap = Free.wrap

class (Comonad w,Functor f) => Coalgebra f w where
  unwrap :: w a -> f (w a)
  corun :: w a -> CofreeT f w a
  corun = coiterT Mop.unwrap
instance (Cofree.ComonadCofree f w) => Coalgebra f w where
  unwrap = Cofree.unwrap

eval :: (ComonadCofree g w,MonadFree f m,Algebra f m,Coalgebra g w,Pairing g f)
     => (a -> b -> m r) -> w a -> FreeT f m b -> m r
eval p cofree free = do
  mf <- runFreeT free
  case mf of
    Pure x -> p (extract cofree) x
    Free ms -> pair (eval p) (Mop.unwrap cofree) ms

eval' :: (ComonadCofree g w,MonadFree f m,Algebra f m,Coalgebra g w,Pairing g f)
      => (a -> b -> m r) -> w (m a) -> FreeT f m b -> m r
eval' p cofree free = do
  mf <- runFreeT free
  a <- extract cofree
  case mf of
    Pure x -> p a x
    Free ms -> pair (eval' p) (Mop.unwrap $ corun cofree) ms

evalC :: (MonadFree f m, ComonadCofree g w, Algebra f m, Coalgebra g w, Pairing g f)
      => (a -> b -> m r) -> CofreeT g w a -> FreeT f m b -> m r
evalC p cofree free = do
  mf <- runFreeT free
  case mf of
    Pure x -> p (extract cofree) x
    Free ms -> pair (evalC p) (Mop.unwrap cofree) ms

evalC' :: (MonadFree f m, ComonadCofree g w, Algebra f m, Coalgebra g w, Pairing g f)
      => (a -> b -> m r) -> CofreeT g w (m a) -> FreeT f m b -> m r
evalC' p cofree free = do
  mf <- runFreeT free
  a <- extract cofree
  case mf of
    Pure x -> p a x
    Free ms -> pair (evalC' p) (Mop.unwrap $ corun cofree) ms

data A k = A k deriving Functor
a = liftF (inj (A ()))
data B k = B Int k deriving Functor
b i = liftF (inj (B i ()))
data C k = C (Int -> k) deriving Functor
c = liftF (inj (C id))

type ABC = A :+: B :+: C

data CoA k = CoA k deriving Functor
data CoB k = CoB (Int -> k) deriving Functor
data CoC k = CoC (Int,k) deriving Functor
instance Pairing CoA A where
  pair p (CoA cok) (A k) = p cok k
instance Pairing CoB B where
  pair p (CoB ik) (B i k) = p (ik i) k
instance Pairing CoC C where
  pair p (CoC (i,k)) (C ik) = p k (ik i)

type CoABC = CoA :*: CoB :*: CoC

coabcCoalgebra :: Comonad w => w a -> CoABC (w a)
coabcCoalgebra = coA *:* coB *:* coC
  where
    coA wa = CoA wa
    coB wa = CoB (\i -> wa)
    coC wa = CoC (1,wa)

type CoABCT = CofreeT CoABC

mkCoABC :: CofreeT CoABC Identity ()
mkCoABC = coiterT coabcCoalgebra (Identity ())

test :: (MonadFree f m, A :<: f, B :<: f, C :<: f) => m Int
test = do
 a
 b 3
 c

x :: (Pairing CoABC f,MonadFree f m,ABC :<: f) => FreeT f m a -> m a
x = eval (\a b -> return b) mkCoABC

main = do
  i <- x test
  print i
