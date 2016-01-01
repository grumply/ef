{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef
  ( main'
  , base
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
import Ef.Lang.Call             as Base

import Ef.Lang.Scoped.Exit      as Base
import Ef.Lang.Scoped.Reflect   as Base
import Ef.Lang.Scoped.Generate  as Base
import Ef.Lang.Scoped.Guard     as Base
import Ef.Lang.Scoped.Try       as Base
import Ef.Lang.Scoped.React     as Base
import Ef.Lang.Scoped.Fiber    as Base
import Ef.Lang.Scoped.Manage    as Base
import Ef.Lang.Scoped.Switch     as Base
import Ef.Lang.Scoped.Vary      as Base
import Ef.Lang.Scoped.Notate    as Base
import Ef.Lang.Scoped.Log       as Base



type Ef
    = '[ Manageable
       , Exitable
       , Gettable
       , Settable
       , Fiberable
       , Switchable
       , Exceptable
       , Guardable
       , Notatable
       , Variable
       , Loggable
       , Reactable
       ]



type Main
    = '[ Managing
       , Exiting
       , Getting
       , Setting
       , Fibering
       , Switching
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



base
    :: Monad parent
    => Object Ef parent

base =
  Object $
      manager
      *:* exiter
      *:* getter
      *:* setter
      *:* fiberer
      *:* switcher
      *:* excepter
      *:* guarder
      *:* notator
      *:* varier
      *:* logger
      *:* reactor
      *:* Empty
