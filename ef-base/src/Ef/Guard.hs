module Ef.Guard (Guard, nondet, Guards(..), guards) where

import Ef
import Data.Foldable

data Guard k
  = Guard Int k
  | FreshScope (Int -> k)
  | forall a. Choose Int [a] (a -> k)
  | Cut Int

instance Functor Guard where
  fmap f (Guard i k) = Guard i (f k)
  fmap f (FreshScope ik) = FreshScope (fmap f ik)
  fmap f (Choose i as ak) = Choose i as (fmap f ak)
  fmap f (Cut i) = Cut i

instance Delta Guard Guard where
  delta eval (Guard i k) (FreshScope ik) = eval k (ik i)

data Guards ms c = Guards
    { choose :: forall f a. Foldable f => f a -> Ef ms c a
    , cut :: forall b. Ef ms c b
    }

nondet :: (Monad c, '[Guard] <. ts) => Guard (Action ts c)
nondet = Guard 0 $ \o ->
  let Module (Guard i k) _ = o
      !i' = succ i
  in pure $ Module (Guard i' k) o
{-# INLINE nondet #-}

guards :: (Monad c, '[Guard] <: ms) => (Guards ms c -> Ef ms c r) -> Ef ms c (Maybe r)
guards l = do
  scope <- Send (FreshScope Return)
  transform id (rewrite scope) $
    Just <$> l Guards
      { choose = \foldable ->
          let list = toList foldable
          in Send (Choose scope list Return)
      , cut = Send (Cut scope)
      }
{-# INLINE guards #-}

rewrite :: (Monad c, '[Guard] <: ms) => Int -> Messages ms (Ef ms c (Maybe r)) -> Ef ms c (Maybe r)
rewrite scope message =
  let ignore = Do (fmap (transform id (rewrite scope)) message)
      check i scoped = if i == scope then scoped else ignore
  in case prj message of
        Just x ->
          case x of
            Choose i as k -> check i $ choosing scope as k (return Nothing)
            _ -> ignore
        Nothing -> ignore

choosing :: (Monad c, '[Guard] <: ms) => Int -> [a] -> (a -> Ef ms c r) -> Ef ms c r -> Ef ms c r
choosing _ [] _ alt = alt
choosing scope (a:as) bp alt = transform id (nestedChoosing scope as alt bp) (bp a)

nestedChoosing :: (Monad c, '[Guard] <: ms)
               => Int
               -> [a]
               -> Ef ms c result
               -> (a -> Ef ms c result)
               -> Messages ms (Ef ms c result)
               -> Ef ms c result
nestedChoosing scope choices alt cContinue message =
    let ignore = Do (fmap (transform id (nestedChoosing scope choices alt cContinue)) message)
        check i scoped =
            if i == scope
                then scoped
                else ignore
    in case prj message of
           Just x ->
               case x of
                   Choose i nestedChoices k -> check i $ choosing scope nestedChoices k (choosing scope choices cContinue alt)
                   Cut i -> check i $ choosing scope choices cContinue alt
                   _ -> ignore
           Nothing -> ignore
