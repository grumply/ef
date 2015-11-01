{-# LANGUAGE RankNTypes, FlexibleContexts, ExistentialQuantification, BangPatterns #-}
module Effect.Reactive where

import Mop.Core
import Mop.IO
import Effect.Exception

import System.Mem.Weak

import Unsafe.Coerce

newtype Event a = Event Int

data Reactive k
  = FreshScope (Int -> k)
  | forall a. Event Int (Event a -> k)
  | forall fs m a b. Register Int (Event a) (a -> Plan fs m b)
  | forall fs m a. Cleanup Int (Event a) (Plan fs m ())
  | forall a. Trigger Int (Event a) a
  | forall a. Unregister Int (Event a)
  | Halt Int

data ReactiveScope fs m = Reactive
  { halt       :: forall a. Plan fs m a
  , event      :: forall a. Plan fs m (Event a)
  , register   :: forall a b. Event a -> (a -> Plan fs m b) -> Plan fs m ()
  , finalizer  :: forall a. Event a -> Plan fs m () -> Plan fs m ()
  , unregister :: forall a. Event a -> Plan fs m ()
  , trigger    :: forall a. Event a -> a -> Plan fs m ()
  }

data ReactiveSubsystem fs m = Subsystem
  { reactions :: [forall a. (Event a,[forall b. a -> Plan fs m b])]
  , cleanup   :: [forall a. (Event a,[Plan fs m ()])]
  }

data Halted = Halted deriving Show
instance Exception Halted

reactive :: forall fs m a. (Has Reactive fs m,Has Throw fs m,MIO m)
         => (    ReactiveScope fs m
              -> Plan fs m a
            )
         -> Plan fs m a
reactive f = do
  scope <- self (FreshScope id)
  transform scope $ f Reactive
    { halt       =              self (Halt       scope)
    , event      =              self (Event      scope id)
    , register   = \ev react -> self (Register   scope ev react)
    , unregister = \ev       -> self (Unregister scope ev)
    , trigger    = \ev a     -> self (Trigger    scope ev a)
    }
  where
    empty = Subsystem []
    transform scope = go 0 empty
      where
        go !subsystem !evC = go'
          where
            go' p =
              case p of
                Step sym bp              ->
                  let ignore = Step sym (\b -> go' (bp b)) in
                  case prj sym of
                    Just x               ->
                      case x of
                        Register i _
                          | i == scope   -> go (addEvent (Event evC) subsystem)
                                               (evC + 1)
                                               (bp (unsafeCoerce ev))
                          | otherwise    -> ignore
                        Unregister i ev
                          | i == scope   -> undefined
                          | otherwise    -> ignore
                        Trigger i ev a
                          | i == scope   -> undefined
                          | otherwise    -> ignore
                        Halt i           -> throw Halted
                    Nothing              -> ignore
                M m -> M (fmap go' m)
                Pure r -> Pure r
