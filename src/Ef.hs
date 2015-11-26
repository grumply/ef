{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# LANGUAGE DataKinds #-}
module Ef
  ( main', base, debug, Ef, Main
  , module Base
  ) where

import Ef.Core                  as Base
import Ef.Data.Promise          as Base
import Ef.Lang.IO               as Base
import Ef.Lang.Fork             as Base
-- import Ef.Lang.Except           as Base
import Ef.Lang.Scoped.Diverge   as Base
import Ef.Lang.Scoped.Exit      as Base
import Ef.Lang.Contract         as Base
-- import Ef.Lang.Scoped.Act       as Base
-- import Ef.Lang.Scoped.Task      as Base
import Ef.Lang.Scoped.Generate  as Base
import Ef.Lang.Scoped.Guard     as Base
import Ef.Lang.Scoped.Try       as Base
import Ef.Lang.Scoped.React     as Base
import Ef.Lang.Scoped.Thread    as Base
import Ef.Lang.Scoped.Manage    as Base
import Ef.Lang.Scoped.Weave     as Base
import Ef.Lang.Scoped.Vary      as Base
import Ef.Lang.Scoped.Notate    as Base
import Ef.Lang.Scoped.Log       as Base

-- Ef language attributes
type Ef
  = '[Manageable
     ,Exitable
--     ,Taskable
     ,Threadable
     ,Weavable
--     ,Exceptable
     ,Guardable
     ,Notatable
     ,Variable
     ,Loggable
     ,Reactable
--     ,Actable
     ,Divergable
     ]

-- Ef language symbols
type Main
  = '[Managing
     ,Exiting
--     ,Tasking
     ,Threading
     ,Weaving
--     ,Excepting
     ,Guarding
     ,Notating
     ,Varying
     ,Logging
     ,Reacting
--     ,Acting
     ,Diverging
     ]

main' :: Pattern Main IO b -> IO b
main' = fmap snd . delta base

debug :: Pattern Main IO b -> IO (Int,b)
debug = fmap snd. deltaDebug base

base :: Monad m => Object Ef m
base = Object $ manager
            *:* exiter
--            *:* tasker
            *:* threader
            *:* weaver
--            *:* excepter
            *:* guarder
            *:* notator
            *:* varier
            *:* logger
            *:* reactor
--            *:* actor
            *:* diverger
            *:* Empty
