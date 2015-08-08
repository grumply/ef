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
  , run', run'', run''', run''''
  ) where

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


run' :: (Functor f,Cofree.ComonadCofree f w,Pairing f g)
     => CofreeT f w a -> Free g b -> (CofreeT f w a,b)
run' w m =
  case runCofreeT w of
    ws -> case runFree m of
            Pure b -> (coiterT unwrap (fmap headF ws),b)
            Free fs -> pair run' (tailF $ extract ws) fs

run'':: (Monad m,Functor f,Cofree.ComonadCofree f w,Pairing f g)
     => CofreeT f w a -> FreeT g m b -> m (CofreeT f w a,b)
run'' w m = do
  mb <- runFreeT m
  case runCofreeT w of
    ws -> case mb of
            Pure b -> return (coiterT unwrap (fmap headF ws),b)
            Free fs -> pair run'' (tailF $ extract ws) fs

run''' :: (Monad m,Functor f,Cofree.ComonadCofree f w,Pairing f g)
       => CofreeT f w (m a) -> Free g b -> m (CofreeT f w (m a),b)
run''' w m = do
  case runCofreeT w of
    ws -> case runFree m of
            Pure b -> return (coiterT unwrap (fmap headF ws),b)
            Free fs -> pair run''' (tailF $ extract ws) fs

data Hole

run'''' :: forall f g m w a b.
           (Functor g,Functor f,Monad m,Comonad w,Pairing f g)
        => CofreeT f w (m a) -> FreeT g m b -> m (CofreeT f w (m a),(a,b))
run'''' w m =
  case runCofreeT w of
    ws -> do
      mb <- runFreeT m
      a <- headF $ extract ws
      case mb of
            Pure b -> return (CofreeT $ f ws,(a,b))
            Free fs -> pair run'''' (tailF $ extract ws) fs
  where
    f :: w (CofreeF f (m a) (CofreeT f w (m a)))
      -> w (CofreeF f (m a) (CofreeT f w (m a)))
    f x = _ x
