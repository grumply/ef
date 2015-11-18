{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Lang.Except.Checked
  ( throwChecked, catchChecked, tryChecked, Throws
  , module Ef.Lang.Except
  ) where

import Ef.Core
import Ef.Lang.Except

import Data.Coerce
import Data.Proxy

{-# INLINE tryChecked #-}
tryChecked :: forall a b fs m . (Monad m, Is Excepting fs m,Exception a)
           => (Throws a => Pattern fs m b)
           -> Pattern fs m (Either a b)
tryChecked a = catchChecked (a >>= \v -> return (Right v)) (\e -> return (Left e))

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
