{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Fork
    ( forkWith
    , forkOSWith
    , forkOnWith
    ) where



import Ef.Core
import Ef.Lang.IO
import Ef.Data.Promise

import qualified Control.Concurrent
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar
import Control.Exception (SomeException(..))
import Control.Monad



newtype ThreadRef attrs parent result =
    ThreadRef (ThreadId,Promise (Object attrs parent,Either SomeException result))



forkWith
    :: forall scope scope' attrs parent parent' result.
       ( (Attrs attrs) `Witnessing` (Symbol scope)
       , Lift IO parent'
       , Monad parent'
       , Monad parent
       )
    => Object attrs parent
    -> Pattern scope parent result
    -> (forall x. parent x -> IO x)
    -> Pattern scope' parent' (ThreadRef attrs parent result)

forkWith comp plan embedInIO =
    do
      p <- io newPromiseIO
      let
        thread =
            embedInIO $ delta comp (try plan)

      tid <- masked $ \restore -> Control.Concurrent.forkIO $
                 do
                   ea <- restore thread
                   void (fulfillIO p ea)

      return $ ThreadRef (tid,p)



forkOSWith
    :: forall scope scope' attrs parent parent' result.
       ( (Attrs attrs) `Witnessing` (Symbol scope)
       , Lift IO parent'
       , Monad parent'
       , Monad parent
       )
    => Object attrs parent
    -> Pattern scope parent result
    -> (forall x. parent x -> IO x)
    -> Pattern scope' parent' (ThreadRef attrs parent result)

forkOSWith comp plan embedInIO =
    do
      p <- io newPromiseIO
      let
        thread =
            embedInIO $ delta comp (try plan)

      tid <- masked $ \restore -> Control.Concurrent.forkOS $
                 do
                   ea <- restore thread
                   void (fulfillIO p ea)

      return $ ThreadRef (tid,p)



forkOnWith
    :: forall scope scope' attrs parent parent' result.
       ( (Attrs attrs) `Witnessing` (Symbol scope)
       , Lift IO parent'
       , Monad parent'
       , Monad parent
       )
    => Int
    -> Object attrs parent
    -> Pattern scope parent result
    -> (forall x. parent x -> IO x)
    -> Pattern scope' parent' (ThreadRef attrs parent result)

forkOnWith n comp plan embedInIO =
    do
      p <- io newPromiseIO
      let
        thread =
            embedInIO $ delta comp (try plan)

      tid <- masked $ \restore -> Control.Concurrent.forkOn n $
                 do
                   ea <- restore thread
                   void (fulfillIO p ea)

      return $ ThreadRef (tid,p)
