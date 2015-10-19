module Effect.Par (fork,forkOS,forkOn) where

import Mop
import Mop.IO
import Data.Promise
import Effect.Exception

import qualified Control.Concurrent
import Control.Concurrent (ThreadId)
import Control.Monad

fork :: forall fs fs' gs m m' a.
        (Pair (Instrs gs) (Symbol fs),Has Throw fs' m',MIO m',Monad m)
     => InstructionsT gs m
     -> PlanT fs m a
     -> (forall x. m x -> IO x)
     -> PlanT fs' m' (ThreadId,Promise (InstructionsT gs m,a))
fork comp plan lft = mio $ do
  p <- newPromiseIO
  tid <- Control.Concurrent.forkIO $ do
           ca <- lft $ delta comp plan
           void $ fulfillIO p ca
  return (tid,p)

forkOS :: forall fs fs' gs m m' a.
          (Pair (Instrs gs) (Symbol fs),Has Throw fs' m',MIO m',Monad m)
       => InstructionsT gs m
       -> PlanT fs m a
       -> (forall x. m x -> IO x)
       -> PlanT fs' m' (ThreadId,Promise (InstructionsT gs m,a))
forkOS comp plan lft = mio $ do
  p <- newPromiseIO
  tid <- Control.Concurrent.forkOS $ do
           ca <- lft $ delta comp plan
           void $ fulfillIO p ca
  return (tid,p)

forkOn :: forall fs fs' gs m m' a.
          (Pair (Instrs gs) (Symbol fs),Has Throw fs' m',MIO m',Monad m)
       => Int
       -> InstructionsT gs m
       -> PlanT fs m a
       -> (forall x. m x -> IO x)
       -> PlanT fs' m' (ThreadId,Promise (InstructionsT gs m,a))
forkOn n comp plan lft = mio $ do
  p <- newPromiseIO
  tid <- Control.Concurrent.forkOn n $ do
           ca <- lft $ delta comp plan
           void $ fulfillIO p ca
  return (tid,p)
