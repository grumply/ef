module Effect.Loop
  ( loop, Loop
  , loops, Loops
  ) where

import Mop
import Data.Function
import Unsafe.Coerce

data Loop k
  = FreshScope (Integer -> k)
  | forall s. Continue Integer s
  | forall a. Break Integer a

data Loops k = Loops Integer k

freshScope :: Has Loop fs m => PlanT fs m Integer
freshScope = symbol (FreshScope id)

loop :: forall fs m s a. Has Loop fs m
     => s
     -> (    (forall b. a -> PlanT fs m b)
          -> (forall b. s -> PlanT fs m b)
          -> s
          -> PlanT fs m a
        )
     -> PlanT fs m a
loop s0 x = do
    scope <- freshScope
    let break a      = symbol (Break scope a)
        continue s   = symbol (Continue scope s)
        loopBody r s = reifyLoop r scope (x break continue s)
    fix loopBody s0

reifyLoop :: Has Loop symbols m
          => (b -> PlanT symbols m a)
          -> Integer
          -> PlanT symbols m a
          -> PlanT symbols m a
reifyLoop restart scope =
  mapStep $ \go (Step syms bp) ->
    let step = Step syms (\b -> go (bp b))
        handle (Continue i s)
          | i == scope = restart (unsafeCoerce s)
          | otherwise = step
        handle (Break i a)
          | i == scope = Pure (unsafeCoerce a)
          | otherwise = step
        handle _ = step
    in maybe step handle (prj syms)

loops :: Uses Loops gs m
      => Instruction Loops gs m
loops = Loops 0 $ \fs ->
  let Loops i k = view fs
  in instruction (Loops (succ i) k) fs

instance Pair Loops Loop where
  pair p (Loops i k) (FreshScope ik) = p k (ik i)
  pair _ _ _ = error "Unscoped looping construct."
