{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Lift where

import           Control.Monad.Trans.Free
import           Control.Comonad.Trans.Cofree

import           Data.Functor.Identity

import           Language.Haskell.TH.Syntax

instance (Lift (f b),Lift a) => Lift (FreeF f a b) where
  lift (Pure x) = [| Pure x |]
  lift (Free fb) = [| Free fb |]

instance (Lift (m (FreeF f a (FreeT f m a)))) => Lift (FreeT f m a) where
  lift (FreeT f) = [| FreeT f |]

instance Lift a => Lift (Identity a) where
  lift (Identity a) = [| Identity a |]
