module Effect.Concurrent (fork,forkOS,forkOn) where

import Mop
import Mop.IO
import Data.Promise
import Effect.Exception

import qualified Control.Concurrent
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar
import Control.Monad

type Ref gs m a = (ThreadId,Promise (Either SomeException (InstructionsT gs m,a)))

fork :: forall fs fs' gs m m' a.
        (Pair (Instrs gs) (Symbol fs)
        ,MIO m'
        ,Has Throw fs' m'
        ,Has Throw fs m
        ) => InstructionsT gs m
          -> PlanT fs m a
          -> (forall x. m x -> IO x)
          -> PlanT fs' m' (Ref gs m a)
fork comp plan lft = do
  p <- mio newPromiseIO
  mv <- mio newEmptyMVar
  (`catch` (\(e :: SomeException) -> void $ fulfill p (Left e))) $ do
    tid <- mio $ Control.Concurrent.forkIO $ do
             ca <- lft $ delta comp $
                    catch (Right <$> plan)
                          (\(e :: SomeException) -> return (Left e))
             void $ fulfillIO p $
               case ca of
                 (gs,esa) ->
                   case esa of
                     Right a -> (Right (gs,a))
                     Left e -> (Left e)
    mio $ putMVar mv tid
  tid <- mio (takeMVar mv)
  return (tid,p)

forkOS :: forall fs fs' gs m m' a.
          (Pair (Instrs gs) (Symbol fs)
          ,MIO m'
          ,Has Throw fs' m'
          ,Has Throw fs m
          ) => InstructionsT gs m
            -> PlanT fs m a
            -> (forall x. m x -> IO x)
            -> PlanT fs' m' (Ref gs m a)
forkOS comp plan lft = do
  p <- mio newPromiseIO
  mv <- mio newEmptyMVar
  (`catch` (\(e :: SomeException) -> void $ fulfill p (Left e))) $ do
    tid <- mio $ Control.Concurrent.forkOS $ do
             ca <- lft $ delta comp $
                    catch (Right <$> plan)
                          (\(e :: SomeException) -> return (Left e))
             void $ fulfillIO p $
               case ca of
                 (gs,esa) ->
                   case esa of
                     Right a -> (Right (gs,a))
                     Left e -> (Left e)
    mio $ putMVar mv tid
  tid <- mio (takeMVar mv)
  return (tid,p)

forkOn :: forall fs fs' gs m m' a.
          (Pair (Instrs gs) (Symbol fs)
          ,MIO m'
          ,Has Throw fs' m'
          ,Has Throw fs m
          ) => Int
            -> InstructionsT gs m
            -> PlanT fs m a
            -> (forall x. m x -> IO x)
            -> PlanT fs' m' (Ref gs m a)
forkOn n comp plan lft = do
  p <- mio newPromiseIO
  mv <- mio newEmptyMVar
  (`catch` (\(e :: SomeException) -> void $ fulfill p (Left e))) $ do
    tid <- mio $ Control.Concurrent.forkOn n $ do
             ca <- lft $ delta comp $
                    catch (Right <$> plan)
                          (\(e :: SomeException) -> return (Left e))
             void $ fulfillIO p $
               case ca of
                 (gs,esa) ->
                   case esa of
                     Right a -> (Right (gs,a))
                     Left e -> (Left e)
    mio $ putMVar mv tid
  tid <- mio (takeMVar mv)
  return (tid,p)
