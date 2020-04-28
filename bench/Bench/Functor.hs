{-# LANGUAGE DataKinds #-}
module Bench.Functor where

import Pure.Bench
import Pure.Test

import Ef
import Data.Functor.Identity
import Control.Monad.Trans.Identity
import Control.Monad.Trans.State

{-# INLINE eval #-}
eval :: Monad c => Ef '[] c a -> c a
eval = foldn return join undefined

ef_fmap_return :: Ef '[] Identity Int
ef_fmap_return = fmap (+1) $ return 3

identity_fmap_return :: Identity Int
identity_fmap_return = fmap (+1) $ return 3

identityT_fmap_return :: IdentityT Identity Int
identityT_fmap_return = fmap (+1) $ return 3

functorSuite :: Test Sync ()
functorSuite = tests [
  fmapReturn,
  fmapLiftReturn
  ]

fmapReturn :: Test Sync ()
fmapReturn = scope "fmap/return" $ do
  br1 <- scope "identity" $ nf runIdentity identity_fmap_return
  notep br1

  br3 <- scope "identityT/identity" $ nf (runIdentity . runIdentityT) identityT_fmap_return
  notep br3

  br2 <- scope "ef/identity" $ nf (runIdentity . eval) ef_fmap_return
  notep br2

ef_fmap_lift_return :: Ef '[] Identity Int
ef_fmap_lift_return = fmap (+1) $ do
  a <- lift (return 'a')
  a `seq` return 3

identityT_fmap_lift_return :: IdentityT Identity Int
identityT_fmap_lift_return = fmap (+1) $ do
  a <- lift (return 'a')
  a `seq` return 3

stateT_fmap_lift_return :: StateT () Identity Int
stateT_fmap_lift_return = fmap (+1) $ do
  a <- lift (return 'a')
  a `seq` return 3

fmapLiftReturn :: Test Sync ()
fmapLiftReturn = scope "fmap/lift/return" $ do
  br <- scope "identityT/identity" $ nf (runIdentity . runIdentityT) identityT_fmap_lift_return
  notep br

  br <- scope "stateT/identity" $ nf (runIdentity . (`evalStateT` ())) stateT_fmap_lift_return
  notep br

  br <- scope "ef/identity" $ nf (runIdentity . eval) ef_fmap_lift_return
  notep br
