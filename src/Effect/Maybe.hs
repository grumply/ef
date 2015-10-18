{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Effect.Maybe
  ( tryMaybe, tryEither, May
  , possible, Possible
  ) where

import Mop
import Unsafe.Coerce
import Data.Maybe

-- Maybe implements short-circuiting plans with success and non-specific failure.

data May k
  = forall a. Success Integer a
  | Failure Integer
  | FreshScope (Integer -> k)

data Possible k = Possible Integer k

possible :: Uses Possible fs m => Instruction Possible fs m
possible = Possible 0 $ \fs ->
  let Possible i k = view fs
  in instruction (Possible (succ i) k) fs

freshScope :: Has May fs m => PlanT fs m Integer
freshScope = symbol (FreshScope id)

-- use: may $ \success failure ...
tryMaybe :: Has May fs m => ((forall b. a -> PlanT fs m b) -> (forall b. PlanT fs m b) -> PlanT fs m (Maybe a)) -> PlanT fs m (Maybe a)
tryMaybe x = do
    scope <- freshScope
    transform scope $ x (\a -> symbol (Success scope a)) (symbol (Failure scope))
  where
    transform scope = go
      where
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Success i a ->
                      if i == scope
                      then return (Just (unsafeCoerce a))
                      else Step sym (\b -> go (bp b))
                    Failure i ->
                      if i == scope
                      then return Nothing
                      else Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                _ -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r

-- unsafe; rewrite this without the Maybe over the scoped result.
tryEither :: Has May fs m => ((forall b. l -> PlanT fs m b) -> (forall b. r -> PlanT fs m b) -> PlanT fs m (Maybe (Either l r))) -> PlanT fs m (Either l r)
tryEither x = fromJust <$> tryMaybe (\success _ -> x (success . Left) (success . Right))

instance Pair Possible May where
  pair p (Possible i k) (FreshScope ik) = p k (ik i)
  pair p _ _ = error "Unscoped try continuation."
