{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Mop.Base
  ( Base, base, run
  , module Base
  ) where

import Mop                  as Base
import Mop.IO               as Base
import Effect.Continuation  as Base
import Effect.Logic         as Base
import Effect.Weave         as Base
import Effect.Exception     as Base
import Effect.Loop          as Base
import Effect.Possibly      as Base
import Effect.Thread        as Base

type Base = '[Continuations,Nondet,Weaving,Throws,Loops,Possible,Threading]
type BaseT fs m = InstructionsT (fs :++: Base) m

base fs = Instructions (fs (continuations *:* nondet *:* weaving *:* throws *:* loops *:* possible *:* threads *:* Empty))

run :: (Monad m,Pair (Instrs Base) (Symbol fs)) => PlanT fs m a -> m a
run = fmap snd . delta (base id)
