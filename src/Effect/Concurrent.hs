module Effect.Concurrent (fork,forkOS,forkOn) where

import Mop
import Mop.IO
import Data.Promise
import Effect.Exception

import qualified Control.Concurrent
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar
import Control.Monad

type Ref gs m a = (ThreadId,Promise (Either SomeException (Object gs m,a)))

fork :: forall fs fs' gs m m' a.
        (Pair (Attrs gs) (Symbol fs)
        ,MIO m'
        ,Has Throw fs' m'
        ,Has Throw fs m
        ) => Object gs m
          -> Plan fs m a
          -> (forall x. m x -> IO x)
          -> Plan fs' m' (Ref gs m a)
fork comp plan lft = do
    p <- io newPromiseIO
    mv <- io newEmptyMVar
    (`catch` (\(e :: SomeException) -> void $ fulfill p (Left e))) $ do
        tid <- io $ Control.Concurrent.forkIO $ do
            ca <- lft $ delta comp $ catch (Right <$> plan)
                (\(e :: SomeException) -> return (Left e))
            void $ fulfillIO p $ case ca of
                (gs,esa) -> case esa of
                    Right a -> (Right (gs,a))
                    Left e -> (Left e)
        io $ putMVar mv tid
    tid <- io (takeMVar mv)
    return (tid,p)

forkOS :: forall fs fs' gs m m' a.
          (Pair (Attrs gs) (Symbol fs)
          ,MIO m'
          ,Has Throw fs' m'
          ,Has Throw fs m
          ) => Object gs m
            -> Plan fs m a
            -> (forall x. m x -> IO x)
            -> Plan fs' m' (Ref gs m a)
forkOS comp plan lft = do
    p <- io newPromiseIO
    mv <- io newEmptyMVar
    (`catch` (\(e :: SomeException) -> void $ fulfill p (Left e))) $ do
        tid <- io $ Control.Concurrent.forkOS $ do
            ca <- lft $ delta comp $ catch (Right <$> plan)
                (\(e :: SomeException) -> return (Left e))
            void $ fulfillIO p $ case ca of
                (gs,esa) -> case esa of
                    Right a -> (Right (gs,a))
                    Left e -> (Left e)
        io $ putMVar mv tid
    tid <- io (takeMVar mv)
    return (tid,p)

forkOn :: forall fs fs' gs m m' a.
          (Pair (Attrs gs) (Symbol fs)
          ,MIO m'
          ,Has Throw fs' m'
          ,Has Throw fs m
          ) => Int
            -> Object gs m
            -> Plan fs m a
            -> (forall x. m x -> IO x)
            -> Plan fs' m' (Ref gs m a)
forkOn n comp plan lft = do
    p <- io newPromiseIO
    mv <- io newEmptyMVar
    (`catch` (\(e :: SomeException) -> void $ fulfill p (Left e))) $ do
        tid <- io $ Control.Concurrent.forkOn n $ do
            ca <- lft $ delta comp $ catch (Right <$> plan)
                (\(e :: SomeException) -> return (Left e))
            void $ fulfillIO p $ case ca of
                (gs,esa) -> case esa of
                    Right a -> (Right (gs,a))
                    Left e -> (Left e)
        io $ putMVar mv tid
    tid <- io (takeMVar mv)
    return (tid,p)
