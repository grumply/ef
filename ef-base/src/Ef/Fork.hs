{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Fork
    ( forkWith
    , forkOSWith
    , forkOnWith
    ) where



import Ef
import Ef.IO
import Data.Promise

import qualified Control.Concurrent
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar
import Control.Exception (SomeException(..))
import Control.Monad



newtype ThreadRef methods super result =
    ThreadRef (ThreadId,Promise (Object methods super,Either SomeException result))


forkWith
    :: ( (Methods methods) `Ma` (Messages messages)
       , Lift IO super'
       , Monad super'
       , Monad super
       )
    => Object methods super
    -> Narrative messages super result
    -> (forall x. super x -> IO x)
    -> Narrative messages' super' (ThreadRef methods super result)

forkWith comp plan embedInIO = do
    p <- io newPromiseIO
    let thread = embedInIO $ delta comp (try plan)
    tid <- masked $ \restore -> Control.Concurrent.forkIO $ do
               ea <- restore thread
               void (fulfillIO p ea)
    return $ ThreadRef (tid,p)


forkOSWith
    :: ( (Methods methods) `Ma` (Messages messages)
       , Lift IO super'
       , Monad super'
       , Monad super
       )
    => Object methods super
    -> Narrative messages super result
    -> (forall x. super x -> IO x)
    -> Narrative messages' super' (ThreadRef methods super result)
    
forkOSWith comp plan embedInIO = do
    p <- io newPromiseIO
    let thread = embedInIO $ delta comp (try plan)
    tid <- masked $ \restore -> Control.Concurrent.forkOS $ do
               ea <- restore thread
               void (fulfillIO p ea)
    return $ ThreadRef (tid,p)


forkOnWith
    :: ( (Methods methods) `Ma` (Messages messages)
       , Lift IO super'
       , Monad super'
       , Monad super
       )
    => Int
    -> Object methods super
    -> Narrative messages super result
    -> (forall x. super x -> IO x)
    -> Narrative messages' super' (ThreadRef methods super result)

forkOnWith n comp plan embedInIO = do
    p <- io newPromiseIO
    let thread = embedInIO $ delta comp (try plan)
    tid <- masked $ \restore -> Control.Concurrent.forkOn n $ do
               ea <- restore thread
               void (fulfillIO p ea)
    return $ ThreadRef (tid,p)
