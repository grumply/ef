{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Lang.Scoped.Task where

import Ef.Core
-- import Lang.Global.IO
-- import Lang.Global.Except

import Control.Concurrent.MVar

data Promise a = Promise
  { getPromiseId :: Int
  , getPromise :: MVar a
  }

-- Priority is the number of steps to perform before yielding automatically.
newtype Priority = Priority Int

-- | Symbols

data Tasking k
  = forall fs m a. Fork Int (Pattern fs m a) (Promise a -> k)
  | forall fs m a. Yield Int k
  | forall fs m a. Demand Int (Promise a) (a -> k)
  | FreshScope (Int -> k)
  | FreshPromiseId (Int -> k)

-- | Symbol Module

data Task fs m = Task
  { fork :: forall a. Pattern fs m a -> Pattern fs m (Promise a)
  , yield :: Pattern fs m ()
  , demand :: forall a. Promise a -> Pattern fs m a
  }

-- | Attribute

data Taskable k = Taskable Int k Int k

-- | Attribute Construct

tasker :: Uses Taskable fs m => Attribute Taskable fs m
tasker = Taskable 0 nextScope 0 nextPromiseId
  where
    nextScope fs =
      let Taskable s ns pid npid = view fs
          s' = succ s
      in s' `seq` pure (fs .= Taskable s' ns pid npid)
    nextPromiseId fs =
      let Taskable s ns pid npid = view fs
          pid' = succ pid
      in pid' `seq` pure (fs .= Taskable s ns pid' npid)

-- | Symbol/Attribute Symmetry

instance Symmetry Taskable Tasking where
  symmetry use (Taskable _ _ pid npidk) (FreshPromiseId pidk) = use npidk (pidk pid)
  symmetry use (Taskable s nsk _ _) (FreshScope sk) = use nsk (sk s)

-- | Local Scoping Construct + Substitution

tasks :: forall fs m a. Is Tasking fs m
      => (    Task fs m
           -> Pattern fs m a
         ) -> Pattern fs m a
tasks f = do
  scope <- self (FreshScope id)
  substitute scope $ f Task
    { fork = \p -> self (Fork scope p id)
    , yield = self (Yield scope ())
    , demand = \p -> self (Demand scope p id)
    }
  where
    substitute scope = go
      where
        go p =
          case p of
            Step sym bp ->
              let check i x = if i == scope then x else ignore
                  ignore = Step sym (\b -> go (bp b))
              in case prj sym of
                   Just x ->
                     case x of
                       Fork i p k -> check i $ _
                       Yield i k -> check i $ _
                       Demand i p k -> check i $ _
                       _ -> ignore
                   _ -> ignore
            M m -> M (fmap go m)
            Pure r -> Pure r
