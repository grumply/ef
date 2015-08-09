{-# LANGUAGE MultiParamTypeClasses #-}
module Interpreter where

import           Control.Monad.Fix

import           Control.Monad.Trans.Free
import           Control.Comonad.Trans.Cofree

import           Control.Monad.Trans

class (Functor g,Monad m) => Interpreter g m where
  int :: FreeT g m a -> m a

instance (MonadFix m,Interpreter f m) => MonadFix (FreeT f m) where
  mfix = lift . mfix . (int .)
