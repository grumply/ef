module Effect.Resource where

import Mop
import Effect.Exception

import Control.Applicative (Applicative(pure, (<*>)))

newtype Resource fs m a = Resource { acquire :: Has Throw fs m => PlanT fs m (a, PlanT fs m ()) }

instance Functor m => Functor (Resource fs m) where
    fmap f resource = Resource $ do
        (a, release) <- acquire resource
        return (f a, release)

instance Functor m => Applicative (Resource fs m) where
    pure a = Resource (pure (a, pure ()))
    resource1 <*> resource2 = Resource $ do
        (f, release1) <- acquire resource1
        (x, release2) <- acquire resource2 `onException` release1
        return (f x, release2 >> release1)

instance Monad m => Monad (Resource fs m) where
    return a = Resource (return (a, return ()))
    m >>= f = Resource $ do
        (m', release1) <- acquire m
        (x , release2) <- acquire (f m') `onException` release1
        return (x, release2 >> release1)

with :: Has Throw fs m => Resource fs m a -> (a -> PlanT fs m ()) -> PlanT fs m ()
with resource k = bracket
  (acquire resource)
  (\(_, release) -> release)
  (\(a, _) -> k a)
