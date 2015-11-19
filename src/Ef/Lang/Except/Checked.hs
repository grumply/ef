{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Lang.Except.Checked
  ( throw, catch, try, Throws
  , module Control.Exception
  , Except.Excepting,Except.excepter
  ) where

import Ef.Core
import qualified Ef.Lang.Except as Except
import Control.Exception (Exception(..),SomeException(..))

import Data.Coerce
import Data.Proxy

{-# INLINE try #-}
try :: forall a b fs m . (Monad m, Is Except.Excepting fs m,Except.Exception a)
           => (Throws a => Pattern fs m b)
           -> Pattern fs m (Either a b)
try a = Ef.Lang.Except.Checked.catch (a >>= \v -> return (Right v)) (\e -> return (Left e))

-- | Symbol Construct

{-# INLINE throw #-}
throw :: (Except.Exception e,Throws e,Is Except.Excepting fs m) => e -> Pattern fs m a
throw = Except.throw

-- | Global Scoping Construct

{-# INLINE catch #-}
catch :: forall e fs m a. (Except.Exception e,Is Except.Excepting fs m)
             => (Throws e => Pattern fs m a)
             -> (e -> Pattern fs m a)
             -> Pattern fs m a
catch act = Except.catch (unthrow (Proxy :: Proxy e) (act :: Throws e => Pattern fs m a))
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
