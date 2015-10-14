{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Effect.Possibly
  ( possibly, Possibly
  , possible, Possible
  ) where

import Mop
import Data.Could

import Unsafe.Coerce

-- Possibly implements short-circuiting plans with success and non-specific failure.

data Possibly k
  = forall a. Success Integer a
  | Failure Integer
  | FreshScope (Integer -> k)

data Possible k = Possible Integer k

possible :: Uses Possible fs m => Instruction Possible fs m
possible = Possible 0 $ \fs ->
  let Possible i k = view fs
  in instruction (Possible (succ i) k) fs

freshScope :: Has Possibly fs m => PlanT fs m Integer
freshScope = symbol (FreshScope id)

-- use: possibly $ \success failure ...
possibly :: Has Possibly fs m => ((forall b. a -> PlanT fs m b) -> (forall b. PlanT fs m b) -> PlanT fs m (Could a)) -> PlanT fs m (Could a)
possibly x = do
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
                then Pure (Did (unsafeCoerce a))
                else Step syms (\b -> go (bp b))
              Failure i ->
                if i == scope
                then Pure Didn't
                else Step syms (\b -> go (bp b))
          Nothing -> Step syms (\b -> go (bp b))

instance Pair Possible Possibly where
  pair p (Possible i k) (FreshScope ik) = p k (ik i)
  pair p _ _ = error "Unscoped try continuation."
