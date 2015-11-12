{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# LANGUAGE DataKinds #-}
module Ef
  ( main', base, debug, Ef, Main
  , module Base
  ) where

import Ef.Core                    as Base
import Data.Promise               as Base
import Lang.Global.IO             as Base
import Lang.Global.Fork           as Base
import Lang.Global.Except         as Base
import Lang.Global.Except.Checked as Base
import Lang.Scoped.Diverge        as Base
import Lang.Scoped.Exit           as Base
import Lang.Contract              as Base
import Lang.Scoped.Act            as Base
import Lang.Scoped.Alternate      as Base
import Lang.Scoped.Generate       as Base
import Lang.Scoped.Guard          as Base
import Lang.Scoped.Try            as Base
import Lang.Scoped.React          as Base
import Lang.Scoped.Thread         as Base
import Lang.Scoped.Manage         as Base
import Lang.Scoped.Weave          as Base
import Lang.Scoped.Vary           as Base
import Lang.Scoped.Notate         as Base
import Lang.Scoped.Log            as Base

-- Ef language attributes
type Ef
  = '[Manageable
     ,Exitable
     ,Alternatable
     ,Threadable
     ,Weavable
     ,Exceptable
     ,Guardable
     ,Notatable
     ,Variable
     ,Loggable
     ,Reactable
     ,Actable
     ,Divergable
     ]

-- Ef language symbols
type Main
  = '[Managing
     ,Exiting
     ,Alternating
     ,Threading
     ,Weaving
     ,Excepting
     ,Guarding
     ,Notating
     ,Varying
     ,Logging
     ,Reacting
     ,Acting
     ,Diverging
     ]

main' :: Pattern Main IO b -> IO b
main' = fmap snd . delta base

debug :: Pattern Main IO b -> IO (Int,b)
debug = fmap snd. deltaDebug base

base :: Monad m => Object Ef m
base = Object $ manager
            *:* exiter
            *:* alternator
            *:* threader
            *:* weaver
            *:* excepter
            *:* guarder
            *:* notator
            *:* varier
            *:* logger
            *:* reactor
            *:* actor
            *:* diverger
            *:* Empty
