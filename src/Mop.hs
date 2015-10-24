{-# LANGUAGE FlexibleInstances #-}
module Mop
  ( main', base, Mop, Main
  , module Base
  ) where

import Mop.Core                   as Base
import Mop.IO                     as Base
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

type Mop
  = '[Transience
     ,Continuations
     ,Nondet
     ,Weaving
     ,Exceptions
     ,Possible
     ,Threading
     ]

type Main
  = '[Transient
     ,Continuation
     ,Logic
     ,Weave
     ,Throw
     ,May
     ,Thread
     ]

main' :: Monad m => Plan Main m b -> m b
main' = fmap snd . _delta base

base :: Monad m => Object Mop m
base = Object $ transience
            *:* continuations
            *:* nondet
            *:* weaving
            *:* exceptions
            *:* possible
            *:* threads
            *:* Empty

-- class Base m m' where
--   base :: Plan Main m a -> m' a
-- instance Base m (Plan Main m) where
--   base = id
-- instance (Functor m',Base m m') => Base m (Plan gs m') where
--   base = lift . base
