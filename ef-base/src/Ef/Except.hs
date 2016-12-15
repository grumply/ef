{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RoleAnnotations #-}
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
import Control.Exception (SomeException,Exception(..))
import Data.Coerce

data Except k
  = Except (SomeException -> k)
  | Throw SomeException
  deriving Functor

instance Delta Except Except where
  delta eval (Except k) (Throw e) = eval (k e) undefined

excepts :: (Monad c, '[Except] .> ts) => Except (Action ts c)
excepts =
    let uncaught err = "Impossible uncaught checked exception: " ++ show err
    in Except (error . uncaught)
{-# INLINE excepts #-}

class Throws e
type role Throws representational

newtype Catch e = Catch e
instance Throws (Catch e)

newtype Wrap e a = Wrap { unWrap :: Throws e => a }

-- | throw a checked exception; the exception must be caught via `catchChecked`
-- before the `Code` can be sent to an `Object`.
throwChecked
    :: (Monad c, '[Except] <: ms, Exception e)
    => e -> (Throws e => Code ms c a)
throwChecked e =
    let exception = toException e
    in Send (Throw exception)

-- | catch a chcked exception created via `throwChecked`; this method must be called
-- on a `Code` carrying a (Throws _ =>) context before it may be sent to an `Object`.
catchChecked
    :: forall e ms c r.
       (Monad c, '[Except] <: ms, Exception e)
    => (Throws e => Code ms c r)
    -> (e -> Code ms c r)
    -> Code ms c r
catchChecked act c =
    let proxy = Proxy :: Proxy e
    in transform id go (unthrow proxy act)
  where
    go m =
      case prj m of
        Just (Throw se) ->
          case fromException se of
            Just e -> c e
            _ -> Do (fmap (transform id go) m)
        _ -> Do (fmap (transform id go) m)
    unthrow :: forall proxy (e :: *) x. proxy e -> (Throws e => x) -> x
    unthrow _ = (unWrap :: Wrap (Catch e) x -> x)
              . (coerceWrap :: forall e. Wrap e x -> Wrap (Catch e) x)
              . (Wrap :: forall e. (Throws e => x) -> Wrap e x)

    coerceWrap :: forall e x. Wrap e x -> Wrap (Catch e) x
    coerceWrap = coerce

-- | similar to `catchChecked` but doesn't handle the exception and instead
-- lifts it into an `Either` sum with the `Codes` expected r.
tryChecked
    :: (Monad c, '[Except] <: ms, Exception e)
    => (Throws e => Code ms c r)
    -> Code ms c (Either e r)
tryChecked a = catchChecked (Right <$> a) (return . Left)

-- | cast a checked exception to another type
mapChecked
    :: (Monad c, '[Except] <: ms, Exception e, Exception e')
    => (e -> e')
    -> (Throws e => Code ms c a)
    -> (Throws e' => Code ms c a)
mapChecked f p = catchChecked p (throwChecked . f)
