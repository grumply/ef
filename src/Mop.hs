{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Product and Pairing were largely the work of Dave Laing and his cofun
series on github at https://github.com/dalaing/cofun and Swierstra's
Data Types a la Carte. Matthew Pickering's mpickering.github.io blog
had a wonderful post about a weaker version of compdata's subsumption/
dependency injection type families that was largely integrated.
-}

module Mop
  ( module Export
  , showFT, showF
  , run,   eval,   exec
  , runT,  evalT,  execT
  , runM,  evalM,  execM
  , runMT, evalMT, execMT
  ) where

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
import Generate    as Export hiding (run)
import Checked     as Export

import Language.Haskell.TH.Syntax

import qualified Control.Comonad.Trans.Cofree as Cofree

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

run :: (Cofree.ComonadCofree f w,Pairing f g)
     => w a -> Free g b -> (w a,b)
run w m = case runFree m of
  Pure x -> (w,x)
  Free gs -> pair run (unwrap w) gs

exec :: (Cofree.ComonadCofree f w, Pairing f g)
     => w a -> Free g b -> a
exec w = extract . fst . run w

eval :: (Cofree.ComonadCofree f w,Pairing f g)
     => w a -> Free g b -> b
eval w = snd . run w



runM :: (Monad m,Cofree.ComonadCofree f w,Pairing f g)
      => w (m a) -> Free g b -> m (w (m a),b)
runM w m = do
  _ <- extract w
  case runFree m of
    Pure x -> return (w,x)
    Free gs -> pair runM (unwrap w) gs

execM :: (Monad m, Cofree.ComonadCofree f w, Pairing f g)
      => w (m a) -> Free g b -> m a
execM w = (extract . fst) <=< runM w

evalM :: (Monad m, Cofree.ComonadCofree f w, Pairing f g)
      => w (m a) -> Free g b -> m b
evalM w = fmap snd . runM w


runT :: (Monad m,Cofree.ComonadCofree f w,Pairing f g)
     => w a -> FreeT g m b -> m (w a,b)
runT w m = do
  mb <- runFreeT m
  case mb of
    Pure x -> return (w,x)
    Free gs -> pair runT (unwrap w) gs

execT :: (Monad m, Cofree.ComonadCofree f w, Pairing f g)
      => w a -> FreeT g m b -> m a
execT w = fmap (extract . fst) . runT w

evalT :: (Monad m, Cofree.ComonadCofree f w, Pairing f g)
      => w a -> FreeT g m b -> m b
evalT w = fmap snd . runT w


runMT :: (Monad m,Cofree.ComonadCofree f w,Pairing f g)
      => w (m a) -> FreeT g m b -> m (w (m a),b)
runMT w m = do
  mb <- runFreeT m
  _ <- extract w
  case mb of
    Pure x -> return (w,x)
    Free gs -> pair runMT (unwrap w) gs

execMT :: (Monad m, Cofree.ComonadCofree f w, Pairing f g)
       => w (m a) -> FreeT g m b -> m a
execMT w = (extract . fst) <=< runMT w

evalMT :: (Monad m, Cofree.ComonadCofree f w, Pairing f g)
       => w (m a) -> FreeT g m b -> m b
evalMT w = fmap snd . runMT w
