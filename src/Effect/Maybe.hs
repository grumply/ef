{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Maybe
    ( tryMaybe, May
    , possible, Possible
    ) where

import Mop.Core

import Unsafe.Coerce

-- Maybe implements short-circuiting plans with success and non-specific failure.

data May k
    = forall a. Success Int a
    | Failure Int
    | FreshScope (Int -> k)

data Possible k = Possible Int k

{-# INLINE possible #-}
possible :: Uses Possible fs m => Attribute Possible fs m
possible = Possible 0 $ \fs ->
    let Possible i k = view fs
    in pure $ fs .= Possible (succ i) k

{-# INLINE freshScope #-}
freshScope :: Has May fs m => Plan fs m Int
freshScope = self (FreshScope id)

{-# INLINE tryMaybe #-}
tryMaybe :: Has May fs m => ((forall b. a -> Plan fs m b) -> (forall b. Plan fs m b) -> Plan fs m (Maybe a)) -> Plan fs m (Maybe a)
tryMaybe x = do
    scope <- freshScope
    transform scope $ x (\a -> self (Success scope a)) (self (Failure scope))
  where
    transform scope = go
      where
        go p = case p of
            Step sym bp -> case prj sym of
                Just x  -> case x of
                    Success i a ->
                        if i == scope
                        then return (Just (unsafeCoerce a))
                        else Step sym (\b -> go (bp b))
                    Failure i ->
                        if i == scope
                        then return Nothing
                        else Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                _ -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r

instance Pair Possible May where
    pair p (Possible i k) (FreshScope ik) = p k (ik i)
    pair p _ _ = error "Unscoped try continuation."
