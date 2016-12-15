module Ef.Fork (forkWith, forkOSWith, forkOnWith) where

import Ef
import Data.Promise
import Control.Exception
import qualified Control.Concurrent
import Control.Concurrent (ThreadId)

newtype ThreadRef ts c r =
    ThreadRef (ThreadId, Promise (Object ts c, r))

forkWith
    :: ((Modules ts) `Delta` (Messages ms), Functor (Messages ms), Functor (Messages ms'), MonadIO c', Monad c', Monad c)
    => Object ts c
    -> Code ms c r
    -> (forall x. c x -> IO x)
    -> Code ms' c' (ThreadRef ts c r)
forkWith comp plan embedInIO = do
    p <- liftIO promise
    let thread = embedInIO $ runWith comp plan
    tid <- liftIO $ mask $ \restore -> Control.Concurrent.forkIO $ do
             ea <- restore thread
             void (fulfill p ea)
    return $ ThreadRef (tid, p)

forkOSWith
    :: ((Modules ts) `Delta` (Messages ms), Functor (Messages ms), Functor (Messages ms'), MonadIO c', Monad c', Monad c)
    => Object ts c
    -> Code ms c r
    -> (forall x. c x -> IO x)
    -> Code ms' c' (ThreadRef ts c r)
forkOSWith comp plan embedInIO = do
    p <- liftIO promise
    let thread = embedInIO $ runWith comp plan
    tid <- liftIO $ mask $ \restore -> Control.Concurrent.forkOS $ do
             ea <- restore thread
             void (fulfill p ea)
    return $ ThreadRef (tid, p)

forkOnWith
    :: ((Modules ts) `Delta` (Messages ms), Functor (Messages ms), Functor (Messages ms'), MonadIO c', Monad c', Monad c)
    => Int
    -> Object ts c
    -> Code ms c r
    -> (forall x. c x -> IO x)
    -> Code ms' c' (ThreadRef ts c r)
forkOnWith n comp plan embedInIO = do
    p <- liftIO promise
    let thread = embedInIO $ runWith comp plan
    tid <- liftIO $ mask $ \restore -> Control.Concurrent.forkOn n $ do
             ea <- restore thread
             void (fulfill p ea)
    return $ ThreadRef (tid, p)
