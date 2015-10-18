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
        loopBody r s = transform scope r (x break continue s)
    fix loopBody s0
  where
    transform scope restart = go
      where
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Continue i s ->
                      if i == scope
                      then restart (unsafeCoerce s)
                      else Step sym (\b -> go (bp b))
                    Break i a ->
                      if i == scope
                      then return (unsafeCoerce a)
                      else Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                _ -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r

loops :: Uses Loops gs m
      => Instruction Loops gs m
loops = Loops 0 $ \fs ->
  let Loops i k = view fs
  in instruction (Loops (succ i) k) fs

instance Pair Loops Loop where
  pair p (Loops i k) (FreshScope ik) = p k (ik i)
  pair _ _ _ = error "Unscoped looping construct."
