{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Effect.Actor where

import Mop
import Mop.IO
import Effect.Exception
import Effect.Weave

import Control.Concurrent
import Data.IORef

type Name = String

data Actor k
  = forall e hs is m'.
    (Pair (Instrs is) (Symbol hs),Has Actor hs m',Exception e)
    => Supervisor
         Name
         (forall x. m' x -> IO x)
         (forall b. PlanT hs m' b -> (e -> PlanT hs m' b) -> PlanT hs m' b)
         (InstructionsT is m')
         k
  | forall e' m'' js ks.
    (Pair (Instrs ks) (Symbol js),Has Actor js m'')
    => Actor
         Name
         (forall x. m'' x -> IO x)
         (forall b. PlanT js m'' b -> (e' -> PlanT js m'' b) -> PlanT js m'' b)
         (InstructionsT ks m'')
         k
  | Stop ((Name,ThreadId) -> k)

data Actors k = Actors (IORef [(Name,ThreadId)]) k

-- | system is the initialization point for an actor system.
--
-- @
-- system supervisionStrategy0 $ \supervisor -> do
--   sObj <- createNewSupervisorObjEffectfully
--   supervisor "someSupervisor" id supervisionStrategy1 sObj $ \actor -> do
--     aObj <- createNewActorObjEffectfully
--     actor "someActor" id aObj
-- @
system :: forall fs m a.
          ( Has Actor fs m, MIO m)
       => (   (    forall e m' hs is. (Pair (Instrs is) (Symbol hs),Has Actor hs m',Exception e)
                => Name
                -> (forall x. m' x -> IO x)
                -> (forall b. PlanT hs m' b -> (e -> PlanT hs m' b) -> PlanT hs m' b)
                -> InstructionsT is m'
                -> PlanT hs m' (Name,ThreadId)
              )
           -> (    forall e' m' m'' hs js ks.
                   (Pair (Instrs ks) (Symbol js),Has Actor hs m',Has Actor js m'',Exception e')
                => Name
                -> (forall x. m'' x -> IO x)
                -> (forall b. PlanT js m'' b -> (e' -> PlanT js m'' b) -> PlanT js m'' b)
                -> InstructionsT ks m''
                -> PlanT hs m' (Name,ThreadId)
               )
            -> PlanT fs m a
          )
       -> PlanT fs m a
system x =
    system_ $
      x (\nm lft strat obj -> symbol (Supervisor nm lft strat obj undefined))
        (\nm lft strat obj -> symbol (Actor nm lft strat obj undefined))
  where
    system_ :: PlanT fs m a -> PlanT fs m a
    system_ p =
      case p of
        Step sym bp ->
          case prj sym of
            Just x ->
              case x of
                Supervisor nm lft strat obj _ -> do
                  undefined
                Actor nm lft strat obj _ ->
                  undefined
        M m -> M (fmap system_ m)
        Pure r -> Pure r
