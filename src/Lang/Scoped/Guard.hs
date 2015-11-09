{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Lang.Scoped.Guard
    ( Guarding, guards
    , Guardable, guarder
    , Guard(..)
    ) where

import Mop.Core
import Lang.Scoped.Weave

import Data.Foldable
import Unsafe.Coerce

-- | Symbol

data Guarding k
    = FreshScope (Int -> k)
    | forall a. Choose Int [a] (a -> k)
    | Cut Int

-- | Symbol Module

data Guard a fs m = Guard
  { choose :: forall f. Foldable f => f a -> Plan fs m a
  , cut :: forall b. Plan fs m b
  , yields :: a -> Plan fs m ()
  }

-- | Attribute

data Guardable k = Guardable Int k

-- | Attribute Construct

{-# INLINE guarder #-}
guarder :: Uses Guardable fs m => Attribute Guardable fs m
guarder = Guardable 0 $ \fs ->
    let Guardable i k = view fs
    in pure $ fs .= Guardable (succ i) k

-- | Symbol/Attribute Symmetry

instance Symmetry Guardable Guarding where
    symmetry use (Guardable i k) (FreshScope ik) = use k (ik i)

-- | Local Scoping Construct

{-# INLINE guards #-}
guards :: forall fs m a.
         (Is Guarding fs m,Is Weaving fs m)
      => (   Guard a fs m
          -> Plan fs m ()
         )
      -> Producer a fs m ()
guards l =
  producer $ \yield -> do
    scope <- self (FreshScope id)
    go scope $ l Guard
      { choose = \as -> self (Choose scope (toList as) id)
      , cut = self (Cut scope)
      , yields = yield
      }
  where
    go scope p0 = go' p0
      where
        try :: forall a. [a] -> (a -> Plan fs m ()) -> Plan fs m () -> Plan fs m ()
        try [] _ or_ = or_
        try (a:as) bp or_ = try' (bp a)
          where
            try' p = case p of
                Step sym bp' -> case prj sym of
                    Just x -> case x of
                        Choose i as' _ ->
                            if i == scope
                            then try as' (unsafeCoerce bp') (try as bp or_)
                            else Step sym (\b -> try' (bp' b))
                        Cut i ->
                            if i == scope
                            then try as bp or_
                            else Step sym (\b -> try' (bp' b))
                    Nothing -> Step sym (\b -> try' (bp' b))
                M m -> M (fmap try' m)
                Pure _ -> try as bp or_
        go' p = case p of
            Step sym bp -> case prj sym of
                Just x -> case x of
                    Choose i as _ ->
                        if i == scope
                        then try as (unsafeCoerce bp) (return ())
                        else Step sym (\b -> go' (bp b))
                    -- ignore cuts if no choices
                    Cut _ -> Step sym (\b -> go' (bp b))
                Nothing -> Step sym (\b -> go' (bp b))
            M m -> M (fmap go' m)
            Pure r -> Pure r
