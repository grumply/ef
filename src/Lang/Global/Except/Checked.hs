{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lang.Global.Except.Checked
  ( throwChecked, catchChecked, Throws
  , module Lang.Global.Except )
  where

import Mop.Core
import Lang.Global.Except

import Data.Coerce
import Data.Proxy

-- | Symbol Construct

{-# INLINE throwChecked #-}
throwChecked :: (Exception e,Throws e,Is Excepting fs m) => e -> Pattern fs m a
throwChecked = throw

-- | Global Scoping Construct

{-# INLINE catchChecked #-}
catchChecked :: forall e fs m a. (Exception e,Is Excepting fs m)
             => (Throws e => Pattern fs m a)
             -> (e -> Pattern fs m a)
             -> Pattern fs m a
catchChecked act = catch (unthrow (Proxy :: Proxy e) (act :: Throws e => Pattern fs m a))
  where
    unthrow :: forall proxy e a. proxy e -> (Throws e => a) -> a
    unthrow _ = unWrap . coerceWrap . Wrap
    coerceWrap :: forall e a. Wrap e a -> Wrap (Catch e) a
    coerceWrap = coerce

class Throws e
type role Throws representational

newtype Catch e = Catch e
instance Throws (Catch e)

newtype Wrap e a = Wrap { unWrap :: Throws e => a }
