{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-
Product and Pairing were largely the work of Dave Laing and his cofun
series on github at https://github.com/dalaing/cofun and Swierstra's
Data Types a la Carte. Matthew Pickering's mpickering.github.io blog
had a wonderful post about a weaker version of compdata's subsumption/
dependency injection type families that was largely integrated.
-}

module Mop (module Export,showFT,showF) where

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

import Language.Haskell.TH.Syntax

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

object = pairEffect (\_ b -> b) . flip coiterT (Identity id)
object' = pairEffect' (\_ b -> b) . flip coiterT (Identity (return ())
