module Effect.Capture where

import Mop

data Capture k' k
  = Checkpoint (k' -> k)
  | Recall k' k
data Reify k' k = Reify (k',k) (k' -> k)

checkpoint :: Monad m => PlanT '[Capture (Cont m a)] m (Cont m a)
checkpoint = sym (Checkpoint id)

recall :: Monad m => Cont m a -> PlanT '[Capture (Cont m a)] m (Cont m a)
recall plan = sym (Recall plan plan)

instance Pair (Reify k) (Capture k) where
  pair p (Reify kl _) (Checkpoint k) = pair p kl k
  pair p (Reify _ kr) (Recall k' k) = pair p kr (k',k)

reify :: Uses (Reify (Cont m a)) symbols m => Cont m a -> Instruction (Reify (Cont m a)) symbols m
reify f = Reify (f,pure) (instr . reify)


newtype Cont m a = Cont { getCont :: PlanT '[Capture (Cont m a)] m a }



-- cont :: Cont m a -> m (Instructions '[Reify (Cont m a)] m,a)
-- cont c = delta (build $ reify c *:* empty) (getCont c)
