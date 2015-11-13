{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Scoped.Try
    ( Try(..), tries
    ) where

import Ef.Core

import Ef.Lang.Scoped.Exit

data Try a fs m = Try
  { success :: forall b. a -> Pattern fs m b
  , failure :: forall b. Pattern fs m b
  }

tries :: Is Exiting fs m => (Try a fs m -> Pattern fs m (Maybe a)) -> Pattern fs m (Maybe a)
tries f = exits $ \exit -> f Try
  { success = \a -> exit (Just a)
  , failure = exit Nothing
  }
