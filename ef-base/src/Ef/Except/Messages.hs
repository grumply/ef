{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Except.Messages
     ( Except(..)
     , Throws
     , throwChecked
     , catchChecked
     , tryChecked
     , mapChecked
     ) where

import Ef.Narrative
import qualified Ef.Exception as Except

import Control.Exception (SomeException,Exception(..))

import Data.Coerce
import Data.Proxy


data Except k = Throw SomeException k

class Throws e
type role Throws representational

newtype Catch e = Catch e
instance Throws (Catch e)

newtype Wrap e a =
    Wrap { unWrap :: Throws e => a }


-- | throw a checked exception; the exception must be caught via `catchChecked`
-- before the `Narrative` can be sent to an `Object`.
throwChecked
    :: (Can Except self, Monad super, Exception e)
    => e -> (Throws e => Narrative self super a)

throwChecked e =
    let exception = toException e
    in self (Throw exception undefined)


-- | catch a chcked exception created via `throwChecked`; this method must be called
-- on a `Narrative` carrying a (Throws _ =>) context before it may be sent to an `Object`.
catchChecked
    :: forall e self super result.
       (Can Except self, Monad super, Exception e)
    => (Throws e => Narrative self super result)
    -> (e -> Narrative self super result)
    -> Narrative self super result

catchChecked act =
    let proxy = Proxy :: Proxy e
    in Except.catch (unthrow proxy act)
  where
    unthrow :: forall proxy e x. proxy e -> (Throws e => x) -> x
    unthrow _ = unWrap . coerceWrap . Wrap

    coerceWrap :: forall e x. Wrap e x -> Wrap (Catch e) x
    coerceWrap = coerce


-- | similar to `catchChecked` but doesn't handle the exception and instead
-- lifts it into an `Either` sum with the `Narratives` expected result.
tryChecked
    :: (Can Except self, Monad super, Exception e)
    => (Throws e => Narrative self super result)
    -> Narrative self super (Either e result)

tryChecked a =
    catchChecked (Right <$> a) (return . Left)


-- | cast a checked exception to another type
mapChecked
    :: (Can Except self, Monad super, Exception e, Exception e')
    => (e -> e')
    -> (Throws e => Narrative self super a)
    -> (Throws e' => Narrative self super a)

mapChecked f p =
    catchChecked p (throwChecked . f)
