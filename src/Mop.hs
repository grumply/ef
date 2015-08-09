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
  , Interpreter(..)
  , showFT, showF
  , run, eval, exec, interp
  , run', eval', exec', interp'
  , Free.toFreeT
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

import Control.Parallel

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

run :: (Monad m, Pairing f g, ComonadCofree f w)
     => w (m a) -> FreeT g m b -> m (CofreeT f w (m a), (a, b))
run w m = w `par` do
  a <- extract w
  mb <- runFreeT m
  a `par` mb `pseq`
    case mb of
      Free fs -> pair run (unwrap w) fs
      Pure b ->
        return
          (CofreeT $ fmap ((:<) (return a) . tailF) $ runCofreeT $ coiterT unwrap w
          ,(a,b)
          )

class (Functor g,Monad m) => Interpreter g m where
  int :: FreeT g m a -> m a

instance (MonadFix m,Interpreter f m) => MonadFix (FreeT f m) where
  mfix = Trans.lift . mfix . (int .)


-- take a w (m a) and return a (FreeT g m b -> m (CofreeT f w (m a),(a,b)))
build = run

exec :: (Monad m, Pairing f g, ComonadCofree f w) => w (m a) -> FreeT g m b -> m a
exec w = fmap (fst . snd) . run w

eval :: (Monad m, Pairing f g, ComonadCofree f w) => w (m a) -> FreeT g m b -> m b
eval w = fmap (snd . snd) . run w

interp :: (Monad m, Pairing f g, ComonadCofree f w)
       => w (m a) -> FreeT g m b -> m (CofreeT f w (m a))
interp w = fmap fst . run w


-- useful because Free.Free has an instance of MonadFix where FreeT cannot.
run' :: (Monad m, Pairing f g, ComonadCofree f w)
     => w (m a) -> Free.Free g b -> m (CofreeT f w (m a),(a,b))
run' w m = do
  a <- extract w
  w `par` a `pseq`
    case m of
      Free.Free fs -> pair run' (unwrap w) fs
      Free.Pure b -> return
        (CofreeT $ fmap ((:<) (return a) . tailF) $ runCofreeT $ coiterT unwrap w
        ,(a,b)
        )

exec' :: (Monad m, ComonadCofree f w, Pairing f g) => w (m b) -> Free.Free g b -> m b
exec'   w = fmap (fst . snd) . run' w

eval' :: (Monad m, ComonadCofree f w, Pairing f g) => w (m a) -> Free.Free g b -> m b
eval'   w = fmap (snd . snd) . run' w

interp' :: (Monad m, ComonadCofree f w, Pairing f g)
        => w (m a) -> Free.Free g b -> m (CofreeT f w (m a))
interp' w = fmap fst . run' w
