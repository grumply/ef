module Mop.Base
  ( Base, base, run
  , module Base
  ) where

import Mop                        as Base
import Mop.IO                     as Base
import Mop.Trans                  as Base
import Data.Promise               as Base
import Effect.Concurrent          as Base
import Effect.Continuation        as Base
import Effect.Exception           as Base
import Effect.List                as Base
import Effect.Logic               as Base
import Effect.Maybe               as Base
import Effect.Thread              as Base
import Effect.Transient           as Base
import Effect.Weave               as Base

type Base = '[Transience,Continuations,Nondet,Weaving,Exceptions,Possible,Threading]
type BaseT fs m = Object (fs :++: Base) m

base fs = Object $ fs $
  transience
  *:* continuations
  *:* nondet
  *:* weaving
  *:* exceptions
  *:* possible
  *:* threads
  *:* Empty

-- simple non-object-based execution; one-offs
run :: (Monad m,Pair (Attrs Base) (Symbol fs)) => Plan fs m a -> m a
run = fmap snd . delta (base id)
