{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Effect.Try
  ( try
  , tries, TryHandler(..)
  ) where

import Mop
import Unsafe.Coerce

-- Try implements short-circuiting plans with success and non-specific failure.

data Try k
  = forall a. Success Integer a
  | Failure Integer
  | FreshScope (Integer -> k)

data TryHandler k = TryHandler Integer k

tries :: Uses TryHandler fs m => Instruction TryHandler fs m
tries = TryHandler 0 $ \fs ->
  let TryHandler i k = view fs
  in instruction (TryHandler (succ i) k) fs

freshScope :: Has Try fs m => Plan fs m Integer
freshScope = symbol (FreshScope id)

try :: Has Try fs m => ((forall b. a -> Plan fs m b) -> (forall b. Plan fs m b) -> Plan fs m (Maybe a)) -> Plan fs m (Maybe a)
try x = do
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
          Nothing -> Step syms (\b -> go (bp b))

instance Pair TryHandler Try where
  pair p (TryHandler i k) (FreshScope ik) = p k (ik i)
  pair p _ _ = error "Unscoped try continuation."
