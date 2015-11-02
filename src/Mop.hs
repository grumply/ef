{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
module Mop
  ( main', base, Mop, Main, Embed(..)
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
import Effect.Thread              as Base
import Effect.Transient           as Base
import Effect.Weave               as Base
import Effect.Divergence          as Base

import Control.Exception          as Base (SomeException,assert)
import Data.Function              as Base (fix)

type Mop
  = '[Transience
     ,Continuations
     ,Interleaving
     ,Nondet
     ,Weaving
     ,Exceptions
     ,Possible
     ,Threading
     ,Divergent
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
     ,Diverge
     ]

main' :: Plan Main IO b -> IO b
main' = fmap snd . delta base

base :: Monad m => Object Mop m
base = Object $ transience
            *:* continuations
            *:* interleaves
            *:* nondet
            *:* weaving
            *:* exceptions
            *:* possible
            *:* threads
            *:* divergent
            *:* Empty

class Monad m => Embed m where
  embed :: Plan Main IO a -> m a
instance Embed IO where
  {-# INLINE embed #-}
  embed = main'
instance Embed (Plan Main IO) where
  {-# INLINE embed #-}
  embed = id
instance (Embed m,Monad m) => Embed (Plan fs m) where
  {-# INLINE embed #-}
  embed = lift . embed
