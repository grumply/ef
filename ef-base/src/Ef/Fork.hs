module Ef.Fork
    ( forkWith
    , forkOSWith
    , forkOnWith
    ) where

import Ef
import Data.Promise

import qualified Control.Concurrent
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar
import Control.Monad

newtype ThreadRef traits super result =
    ThreadRef (ThreadId,Promise (Object traits super,Either SomeException result))

forkWith
    :: ( (Traits traits) `Ma` (Messages messages)
       , MonadIO super'
       , MonadThrow super
       , Monad super'
       , Monad super
       )
    => Object traits super
    -> Narrative messages super result
    -> (forall x. super x -> IO x)
    -> Narrative messages' super' (ThreadRef traits super result)

forkWith comp plan embedInIO = do
    p <- liftIO promise
    let thread = embedInIO $ delta comp (try plan)
    tid <- liftIO $ mask $ \restore -> Control.Concurrent.forkIO $ do
               ea <- restore thread
               void (fulfill p ea)
    return $ ThreadRef (tid,p)

forkOSWith
    :: ( (Traits traits) `Ma` (Messages messages)
       , MonadIO super'
       , MonadThrow super
       , Monad super'
       , Monad super
       )
    => Object traits super
    -> Narrative messages super result
    -> (forall x. super x -> IO x)
    -> Narrative messages' super' (ThreadRef traits super result)

forkOSWith comp plan embedInIO = do
    p <- liftIO promise
    let thread = embedInIO $ delta comp (try plan)
    tid <- liftIO $ mask $ \restore -> Control.Concurrent.forkOS $ do
               ea <- restore thread
               void (fulfill p ea)
    return $ ThreadRef (tid,p)

forkOnWith
    :: ( (Traits traits) `Ma` (Messages messages)
       , MonadIO super'
       , MonadThrow super
       , Monad super'
       , Monad super
       )
    => Int
    -> Object traits super
    -> Narrative messages super result
    -> (forall x. super x -> IO x)
    -> Narrative messages' super' (ThreadRef traits super result)
forkOnWith n comp plan embedInIO = do
    p <- liftIO promise
    let thread = embedInIO $ delta comp (try plan)
    tid <- liftIO $ mask $ \restore -> Control.Concurrent.forkOn n $ do
               ea <- restore thread
               void (fulfill p ea)
    return $ ThreadRef (tid,p)
