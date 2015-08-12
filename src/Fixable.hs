{-# LANGUAGE MultiParamTypeClasses #-}
module Fixable where

import           Control.Monad.Fix

import           Control.Monad.Trans.Free
import           Control.Comonad.Trans.Cofree

import           Control.Monad.Trans

class (Functor g,Monad m) => Fixable g m where
  fixable :: FreeT g m a -> m a

instance (MonadFix m,Fixable f m) => MonadFix (FreeT f m) where
  mfix = lift . mfix . (fixable .)
