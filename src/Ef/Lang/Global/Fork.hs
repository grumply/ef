{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-
TODO: I should clean this up. Not happy with the current interface; I would
      like to tie this in with Async and Divergence for a better story here.
-}
module Ef.Lang.Global.Fork (forkWith,forkOSWith,forkOnWith) where

import Ef.Core
import Ef.Lang.Global.IO
import Ef.Lang.Global.Except
import Ef.Data.Promise

import qualified Control.Concurrent
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar
import Control.Monad

type Ref gs m a = (ThreadId,Promise (Either SomeException (Object gs m,a)))

forkWith :: forall fs fs' gs m m' a.
        (Symmetry (Attrs gs) (Symbol fs)
        ,Lift IO m'
        ,Is Excepting fs' m'
        ,Is Excepting fs m
        ) => Object gs m
          -> Pattern fs m a
          -> (forall x. m x -> IO x)
          -> Pattern fs' m' (Ref gs m a)
forkWith comp plan lft = do
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

forkOSWith :: forall fs fs' gs m m' a.
          (Symmetry (Attrs gs) (Symbol fs)
          ,Lift IO m'
          ,Is Excepting fs' m'
          ,Is Excepting fs m
          ) => Object gs m
            -> Pattern fs m a
            -> (forall x. m x -> IO x)
            -> Pattern fs' m' (Ref gs m a)
forkOSWith comp plan lft = do
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

forkOnWith :: forall fs fs' gs m m' a.
          (Symmetry (Attrs gs) (Symbol fs)
          ,Lift IO m'
          ,Is Excepting fs' m'
          ,Is Excepting fs m
          ) => Int
            -> Object gs m
            -> Pattern fs m a
            -> (forall x. m x -> IO x)
            -> Pattern fs' m' (Ref gs m a)
forkOnWith n comp plan lft = do
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
