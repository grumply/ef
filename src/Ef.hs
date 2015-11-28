{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef
  ( main'
  , base
  , debug
  , Ef
  , Main
  , module Base
  ) where



import Ef.Core                  as Base

import Ef.Data.Promise          as Base

import Ef.Lang.IO               as Base
import Ef.Lang.Fork             as Base
import Ef.Lang.Checked          as Base
import Ef.Lang.Contract         as Base
import Ef.Lang.Get              as Base
import Ef.Lang.Set              as Base

import Ef.Lang.Scoped.Call      as Base
import Ef.Lang.Scoped.Exit      as Base
import Ef.Lang.Scoped.Reflect   as Base
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



type Ef
    = '[ Manageable
       , Callable
       , Exitable
       , Gettable
       , Settable
       , Threadable
       , Weavable
       , Exceptable
       , Guardable
       , Notatable
       , Variable
       , Loggable
       , Reactable
       ]



type Main
    = '[ Managing
       , Calling
       , Exiting
       , Getting
       , Setting
       , Threading
       , Weaving
       , Excepting
       , Guarding
       , Notating
       , Varying
       , Logging
       , Reacting
       ]

main'
    :: Pattern Main IO b
    -> IO b

main' =
    fmap snd . delta base



debug
    :: Pattern Main IO b
    -> IO (Int,b)

debug =
    fmap snd. deltaDebug base



base
    :: Monad m
    => Object Ef m

base =
  Object $
      manager
      *:* caller
      *:* exiter
      *:* getter
      *:* setter
      *:* threader
      *:* weaver
      *:* excepter
      *:* guarder
      *:* notator
      *:* varier
      *:* logger
      *:* reactor
      *:* Empty
