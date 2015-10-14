{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Continuation
  (Continuation,continuation
  ,Continuations,continuations
  ) where

import Mop
import Unsafe.Coerce

data Continuation k
  = FreshScope (Integer -> k)
  | forall a. Continuation Integer a
data Continuations k = Continuations Integer k

continuation :: Has Continuation fs m => ((forall b. a -> PlanT fs m b) -> PlanT fs m a) -> PlanT fs m a
continuation x = do
    scope <- freshScope
    transform scope $ x (\a -> symbol (Continuation scope a))
  where
    transform scope =
      mapStep $ \go (Step syms bp) ->
        case prj syms of
          Just (Continuation i a) ->
            if i == scope
            then Pure (unsafeCoerce a)
            else Step syms (\b -> go (bp b))
          _   -> Step syms (\b -> go (bp b))

freshScope :: Has Continuation fs m => PlanT fs m Integer
freshScope = symbol (FreshScope id)

continuations :: Uses Continuations gs m => Instruction Continuations gs m
continuations = Continuations 0 $ \fs ->
  let Continuations i k = view fs
  in instruction (Continuations (succ i) k) fs

instance Pair Continuations Continuation where
  pair p (Continuations i k) (FreshScope ik) = p k (ik i)
  pair p _ (Continuation _ _) = error "Unscoped continuation."
