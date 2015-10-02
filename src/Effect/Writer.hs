{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

tell :: (Has (Writer w) fs m) => w -> Plan fs m ()
tell w = symbol (Writer w ())

writer :: (Monoid w,Uses (Tracer w) fs m) => Instruction (Tracer w) fs m
writer = tracer mempty (<>)

tracer :: Uses (Tracer w) fs m => w -> (w -> w -> w) -> Instruction (Tracer w) fs m
tracer w0 f = Tracer w0 $ \w' fs ->
  let Tracer w k = view fs
  in instruction (Tracer (f w w') k) fs

written :: Uses (Tracer w) fs m => Instructions fs m -> w
written fs = let Tracer w _ = view fs in w

data Squelch w k = Squelch (w -> w -> w) k
data Squelcher w k = Squelcher (w -> w -> w,k)

squelcher :: forall fs w m.
             (Uses (Tracer w) fs m,Uses (Squelcher w) fs m)
          => Instruction (Squelcher w) fs m
squelcher = Squelcher $ \fs ->
  let t@(Tracer w k) = view fs
  in instruction (Tracer w (\_ -> pure) :: Instruction (Tracer w) fs m) fs
