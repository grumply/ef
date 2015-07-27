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
{-
Product and Pairing were largely the work of Dave Laing and his cofun
series on github at https://github.com/dalaing/cofun and Swierstra's
Data Types a la Carte. Matthew Pickering's mpickering.github.io blog
had a wonderful post about a weaker version of compdata's subsumption/
dependency injection type families that was largely integrated.
-}

module Mop (module Export,showFT,showF) where

import Control.Monad as Export
import Control.Comonad as Export

import Control.Monad.Trans.Free as Export
import Control.Comonad.Trans.Cofree as Export

import Control.Comonad.Store    as Export
import Control.Comonad.Env      as Export
import Control.Comonad.Identity as Export
import Control.Comonad.Traced   as Export hiding (Sum(..),Product(..))

import Sum         as Export
import Product     as Export
import Subsumption as Export
import Pairing     as Export
import Optimize    as Export
import Synonyms    as Export
import Generate    as Export

import Language.Haskell.TH.Syntax
import Control.Monad.IO.Class

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

-- run :: (Functor f, Comonad w, Combines (CofreeT f w a) b r)
--   => w a -> (w a -> f (w a)) -> b -> r
-- run start next = combine (coiterT next start)

-- object
--   :: (Functor f, Combines (CofreeT f Identity (a -> a)) b r) =>
--      (Identity (a -> a) -> f (Identity (a -> a))) -> b -> r
-- object = run (Identity id)

-- object'
--   :: (Monad m, Functor f,
--       Combines (CofreeT f Identity (a -> m a)) b r) =>
--      (Identity (a -> m a) -> f (Identity (a -> m a))) -> b -> r
-- object' = run (Identity return)

{-

data CoA k = CoA (String -> k)
data CoB k = CoB k
data CoC k = CoC (String,k)
data CoD s k = CoD (s -> (String,k))
type CoAlg s = CoA :*: CoB :*: CoC :*: CoD s
-}

data A k = A k deriving Functor
a :: (A :<: f,MonadFree f m) => m ()
a = liftF (inj (A ()))

data B k = B (String -> k) deriving Functor
b :: (B :<: f,MonadFree f m) => m String
b = liftF (inj (B id))

type AB = A :+: B

data CoA k = CoA k deriving Functor

coA = CoA
instance Functor m => PairingM CoA A m where
  pairM (CoA cok) (A k) = combineM cok k

data CoB k = CoB (String,k) deriving Functor
coB :: k -> CoB k
coB wa = CoB (undefined,wa)
instance Functor m => PairingM CoB B m where
  pairM (CoB (s,cok)) (B sk) = combineM cok (sk s)

type CoAB = CoA :*: CoB

mkcoab = coiterT (coA *:* coB) (Identity ())

test :: (MonadIO m,MonadFree AB m) => m ()
test = do
  a
  b
  a


r :: IO ()
r = pairEffectM mkcoab test

main = do
  print =<< r
  return ()
