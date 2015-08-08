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
  , runM, runM'
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

import qualified Debug.Trace as Debug

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

runM :: (Monad m, Functor g, Pairing f g, ComonadCofree f w)
     => w (m a) -> FreeT g m b -> m (CofreeT f w (m a), (a, b))
runM w m = do
  a <- extract w
  mb <- runFreeT m
  case mb of
    Free fs -> pair runM (unwrap w) fs
    Pure b -> return $ (CofreeT $ fmap (\t -> (return a) :< tailF t)
                                               -- ^ remove effects
                                    $ runCofreeT
                                    $ coiterT unwrap w
                       ,(a,b))

execM :: (Monad m, Functor g, Pairing f g, ComonadCofree f w)
      => w (m a) -> FreeT g m b -> m a
execM w = fmap (fst . snd) . runM w

evalM :: (Monad m, Functor g, Pairing f g, ComonadCofree f w)
      => w (m a) -> FreeT g m b -> m b
evalM w = fmap (snd . snd) . runM w

interpM :: (Monad m, Functor g, Pairing f g, ComonadCofree f w)
        => w (m a) -> FreeT g m b -> m (CofreeT f w (m a))
interpM w = fmap fst . runM w
