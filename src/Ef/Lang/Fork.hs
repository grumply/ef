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



newtype ThreadRef gs m a =
    ThreadRef (ThreadId,Promise (Object gs m,Either SomeException a))



forkWith
    :: forall fs fs' gs m m' a.
       ( (Attrs gs) `Witnessing` (Symbol fs)
       , Lift IO m'
       , Monad m'
       , Monad m
       )
    => Object gs m
    -> Pattern fs m a
    -> (forall x. m x -> IO x)
    -> Pattern fs' m' (ThreadRef gs m a)

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
    :: forall fs fs' gs m m' a.
       ( Witnessing (Attrs gs) (Symbol fs)
       , Lift IO m'
       , Monad m'
       , Monad m
       )
    => Object gs m
    -> Pattern fs m a
    -> (forall x. m x -> IO x)
    -> Pattern fs' m' (ThreadRef gs m a)

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
    :: forall fs fs' gs m m' a.
       ( (Attrs gs) `Witnessing` (Symbol fs)
       , Lift IO m'
       , Monad m'
       , Monad m
       )
    => Int
    -> Object gs m
    -> Pattern fs m a
    -> (forall x. m x -> IO x)
    -> Pattern fs' m' (ThreadRef gs m a)

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
