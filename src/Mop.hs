{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# LANGUAGE DataKinds #-}
module Mop
  ( main', base, debug, Mop, Main
  , module Base
  ) where

import Mop.Core                   as Base
import Mop.IO                     as Base
import Data.Promise               as Base
import Effect.Concurrent          as Base
import Effect.Continuation        as Base
import Effect.Contract            as Base
import Effect.Exception           as Base
import Effect.Interleave          as Base
import Effect.List                as Base
import Effect.Logic               as Base
import Effect.Maybe               as Base
import Effect.Reactive            as Base
import Effect.Thread              as Base
import Effect.Transient           as Base
import Effect.Weave               as Base
import Effect.Divergence          as Base

import Effect.Local.State         as Base
import Effect.Local.Writer        as Base
import Effect.Local.Journaler     as Base

type Mop
  = '[Transience
     ,Continuations
     ,Interleaving
     ,Nondet
     ,Weaving
     ,Exceptions
     ,Possible
     ,Threading
     ,Reactive
     ,Divergent
     ,Store
     ,Logger
     ,Journaling
     ]

type Main
  = '[Transient
     ,Continuation
     ,Interleave
     ,Logic
     ,Weave
     ,Throw
     ,May
     ,Thread
     ,React
     ,Diverge
     ,State
     ,Writer
     ,Journaler
     ]

main' :: Plan Main IO b -> IO b
main' = fmap snd . delta base

debug :: Plan Main IO b -> IO (Int,b)
debug = fmap snd. deltaDebug base

base :: Monad m => Object Mop m
base = Object $ transience
            *:* continuations
            *:* interleaves
            *:* nondet
            *:* weaving
            *:* exceptions
            *:* possible
            *:* threads
            *:* reactive
            *:* divergent
            *:* store
            *:* logger
            *:* journaling
            *:* Empty
