{-# language RecursiveDo #-}
{-# language UndecidableInstances #-}
{-# language InstanceSigs #-}
module Ef.Base (module Ef.Base,module X) where

import Ef as X

import Ef.Sync as X
import Ef.Contract as X
import Ef.Event as X
import Ef.Except as X
import Ef.Exit as X
import Ef.Fiber as X
import Ef.Fork as X
import Ef.Manage as X
import Ef.Note as X
import Ef.Reader as X
import Ef.State as X
import Ef.Var as X
import Ef.Writer as X

import Data.Promise as X
import Data.Queue as X

import System.Random.Shuffle as X

import Control.Monad as X

liftIO_ :: MonadIO m => IO a -> m ()
liftIO_ = void . liftIO

lift_ :: (Monad m, Functor (t m), MonadTrans t) => m a -> t m ()
lift_ = void . lift

super_ :: Functor super => super (Narrative self super a) -> Narrative self super ()
super_ = void . super
