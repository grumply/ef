{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Scoped.Act
  ( Acting
  , Actable, actor
  , Act(..)
  ) where

import Ef.Core

data Acting k = Acting k

data Act fs m = Act

data Actable k = Actable k

actor :: Uses Actable gs m => Attribute Actable gs m
actor = Actable return

instance Symmetry Actable Acting where
  symmetry use (Actable k) (Acting k') = use k k'
