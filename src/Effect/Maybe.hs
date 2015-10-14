{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Effect.Maybe
  ( may, May
  , possible, Possible
  ) where

import Mop
import Unsafe.Coerce

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
may :: Has May fs m => ((forall b. a -> PlanT fs m b) -> (forall b. PlanT fs m b) -> PlanT fs m (Maybe a)) -> PlanT fs m (Maybe a)
may x = do
    scope <- freshScope
    transform scope $ x (\a -> symbol (Success scope a)) (symbol (Failure scope))
  where
    transform scope =
      mapStep $ \go stp@(Step syms bp) ->
        case prj syms of
          Just tried ->
            case tried of
              Success i a ->
                if i == scope
                then Pure (Just (unsafeCoerce a))
                else Step syms (\b -> go (bp b))
              Failure i ->
                if i == scope
                then Pure Nothing
                else Step syms (\b -> go (bp b))
              _ -> Step syms (\b -> go (bp b))
          Nothing -> Step syms (\b -> go (bp b))

instance Pair Possible May where
  pair p (Possible i k) (FreshScope ik) = p k (ik i)
  pair p _ _ = error "Unscoped try continuation."
