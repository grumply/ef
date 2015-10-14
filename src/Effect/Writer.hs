module Effect.Writer
  ( Writer, tell
  , Tracer, tracer, writer, written
  ) where

import Mop

import Data.Monoid

data Writer r k = Writer r k

data Tracer r k = Tracer r (r -> k)

instance Pair (Tracer r) (Writer r) where
  pair p (Tracer _ rk) (Writer r k) = pair p rk (r,k)

tell :: (Has (Writer w) fs m) => w -> PlanT fs m ()
tell w = symbol (Writer w ())

writer :: (Monoid w,Uses (Tracer w) fs m) => Instruction (Tracer w) fs m
writer = tracer mempty (<>)

tracer :: Uses (Tracer w) fs m => w -> (w -> w -> w) -> Instruction (Tracer w) fs m
tracer w0 f = Tracer w0 $ \w' is ->
  let Tracer w k = view is
  in instruction (Tracer (f w w') k) is

written :: Uses (Tracer w) fs m => InstructionsT fs m -> w
written fs = let Tracer w _ = view fs in w
