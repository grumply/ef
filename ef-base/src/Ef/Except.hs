{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Except
     ( Except(..)
     , Throws
     , throwChecked
     , catchChecked
     , tryChecked
     , mapChecked
     , excepts
     ) where

import Ef

import qualified Ef.Exception as Except

import Control.Exception (SomeException,Exception(..))

import Data.Coerce
import Data.Proxy

data Except k
  = Except (SomeException -> k)
  | Throw SomeException k

instance Ma Except Except where
    ma use (Except k) (Throw e k') = use (k e) k'

excepts :: (Monad super, '[Except] .> traits)
        => Trait Except traits super
excepts =
    let uncaught err = "Impossible uncaught checked exception: " ++ show err
    in Except (error . uncaught)
{-# INLINE excepts #-}

class Throws e
type role Throws representational

newtype Catch e = Catch e
instance Throws (Catch e)

newtype Wrap e a =
    Wrap { unWrap :: Throws e => a }

-- | throw a checked exception; the exception must be caught via `catchChecked`
-- before the `Narrative` can be sent to an `Object`.
throwChecked
    :: (Monad super, '[Except] :> self, Exception e)
    => e -> (Throws e => Narrative self super a)
throwChecked e =
    let exception = toException e
    in self (Throw exception undefined)

-- | catch a chcked exception created via `throwChecked`; this method must be called
-- on a `Narrative` carrying a (Throws _ =>) context before it may be sent to an `Object`.
catchChecked
    :: forall e self super result.
       (Monad super, '[Except] :> self, Exception e)
    => (Throws e => Narrative self super result)
    -> (e -> Narrative self super result)
    -> Narrative self super result
catchChecked act =
    let proxy = Proxy :: Proxy e
    in Except.catch (unthrow proxy act)
  where
    unthrow :: forall proxy e x. proxy e -> (Throws e => x) -> x
    unthrow _ = (unWrap :: Wrap (Catch e) x -> x)
              . (coerceWrap :: forall e. Wrap e x -> Wrap (Catch e) x)
              . (Wrap :: forall e. (Throws e => x) -> Wrap e x)

    coerceWrap :: forall e x. Wrap e x -> Wrap (Catch e) x
    coerceWrap = coerce

-- | similar to `catchChecked` but doesn't handle the exception and instead
-- lifts it into an `Either` sum with the `Narratives` expected result.
tryChecked
    :: (Monad super, '[Except] :> self, Exception e)
    => (Throws e => Narrative self super result)
    -> Narrative self super (Either e result)
tryChecked a = catchChecked (Right <$> a) (return . Left)

-- | cast a checked exception to another type
mapChecked
    :: (Monad super, '[Except] :> self, Exception e, Exception e')
    => (e -> e')
    -> (Throws e => Narrative self super a)
    -> (Throws e' => Narrative self super a)
mapChecked f p = catchChecked p (throwChecked . f)
