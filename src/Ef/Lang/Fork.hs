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



newtype ThreadRef attrs environment result =
    ThreadRef (ThreadId,Promise (Object attrs environment,Either SomeException result))



forkWith
    :: forall lexicon lexicon' attrs environment environment' result.
       ( (Attrs attrs) `Inflection` (Lexeme lexicon)
       , Lift IO environment'
       , Monad environment'
       , Monad environment
       )
    => Object attrs environment
    -> Narrative lexicon environment result
    -> (forall x. environment x -> IO x)
    -> Narrative lexicon' environment' (ThreadRef attrs environment result)

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
    :: forall lexicon lexicon' attrs environment environment' result.
       ( (Attrs attrs) `Inflection` (Lexeme lexicon)
       , Lift IO environment'
       , Monad environment'
       , Monad environment
       )
    => Object attrs environment
    -> Narrative lexicon environment result
    -> (forall x. environment x -> IO x)
    -> Narrative lexicon' environment' (ThreadRef attrs environment result)

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
    :: forall lexicon lexicon' attrs environment environment' result.
       ( (Attrs attrs) `Inflection` (Lexeme lexicon)
       , Lift IO environment'
       , Monad environment'
       , Monad environment
       )
    => Int
    -> Object attrs environment
    -> Narrative lexicon environment result
    -> (forall x. environment x -> IO x)
    -> Narrative lexicon' environment' (ThreadRef attrs environment result)

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
