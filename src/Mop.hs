{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-
Product and Pairing were largely the work of Dave Laing and his cofun
series on github at https://github.com/dalaing/cofun and Swierstra's
Data Types a la Carte.

Modifications have been made to Subsumption to integrate a blog post that
described a weaker version of what compdata implements. I have been
unable to find it again for attribution. If you've seen the blog post,
please send me an email to sean@grump.ly
-}

module Mop (module Export) where

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

import Language.Haskell.TH.Syntax

instance (Lift (f b),Lift a) => Lift (FreeF f a b) where
  lift (Pure x) = [| Pure x |]
  lift (Free fb) = [| Free fb |]

instance (Lift (m (FreeF f a (FreeT f m a)))) => Lift (FreeT f m a) where
  lift (FreeT f) = [| FreeT f |]

instance Lift a => Lift (Identity a) where
  lift (Identity a) = [| Identity a |]
