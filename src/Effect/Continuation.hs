module Effect.Continuation
  (Continuation,enter
  ,Continuations,continuations
  ) where

import Mop
import Unsafe.Coerce

data Continuation k
  = FreshScope (Integer -> k)
  | forall a. Continuation Integer a
data Continuations k = Continuations Integer k

-- use: enter $ \exit -> do { .. ; }
enter :: Has Continuation fs m => ((forall b. a -> PlanT fs m b) -> PlanT fs m a) -> PlanT fs m a
enter x = do
    scope <- freshScope
    transform scope $ x (\a -> symbol (Continuation scope a))
  where
    transform scope = go
      where
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Continuation i a ->
                      if i == scope
                      then return (unsafeCoerce a)
                      else Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                _ -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r

freshScope :: Has Continuation fs m => PlanT fs m Integer
freshScope = symbol (FreshScope id)

continuations :: Uses Continuations gs m => Instruction Continuations gs m
continuations = Continuations 0 $ \fs ->
  let Continuations i k = view fs
  in instruction (Continuations (succ i) k) fs

instance Pair Continuations Continuation where
  pair p (Continuations i k) (FreshScope ik) = p k (ik i)
  pair p _ (Continuation _ _) = error "Unscoped continuation."
