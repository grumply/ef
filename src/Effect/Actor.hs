module Effect.Actor where

import Control.Concurrent

import Mop
import Mop.IO
import Effect.Weave

type Name = String

data ActorPrims k
  = FreshScope (Integer -> k)
  | System k                     -- create an actor system
  | forall m m' m'' gs fs hs.
    Pair (Instrs gs) (Symbol fs) =>
    Supervisor
      Name
      (forall x. m' x -> IO x)
      (InstructionsT gs m')
      (forall b. PlanT fs m b -> PlanT fs m b)
      (    Name
        -- ^ Actor name
        -> (forall x. m'' x -> IO x)
        -- ^ lifting function for our actor;
        --   how to embed the actor into IO.
        -> InstructionsT hs m''
        -- ^ Actor object
        -> (forall b. PlanT hs m' b -> PlanT hs m' b)
        -- ^ Supervision strategy applied to
        --   every execution of the actor.
        -> PlanT gs m' (Name,ThreadId)
        -- ^ Encapsulated Name, ThreadId and Inbox return
      )
      k
  | New k                        -- create a new supervised actor
  | Unsupervised k               -- create an unsupervised actor
  | Stop (Name,ThreadId) k -- stop an actor

data ActorSystem k =
  ActorSystem k

-- | system use:
-- >    system $ \supervisor rcv -> do
-- >      -- Creates a scoped system with a function capable of
-- >      -- instantiating a supervisor and a function capable
-- >      -- of reading messages from a some inbox.
-- >      supervisor "someSupervisor" id supervisorObj supervision $ \supervised -> do
-- >        -- Creates a supervisor with the following properties:
-- >        -- Name: `"someSupervisor"`
-- >        -- Lifting of effects: `id` specifies execution in IO and that the
-- >        --                     entire system is executing in IO.
-- >        -- Supervisor: `supervisorObj` is the specific supervisor to be run.
-- >        -- Supervision strategy: `supervision`
-- >        supervised "someActor" $ \rcv -> do
--
