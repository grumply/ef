{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Lang.Scoped.Try
    ( tries, Trying
    , trier, Tryable
    , Try(..)
    ) where

import Mop.Core

import Unsafe.Coerce

-- | Symbols

data Trying k
    = forall a. Success Int a
    | Failure Int
    | FreshScope (Int -> k)

-- | Symbol Module

data Try a fs m = Try
  { success :: forall b. a -> Pattern fs m b
  , failure :: forall b. Pattern fs m b
  }

-- | Attribute

data Tryable k = Tryable Int k

-- | Attribute Construct

{-# INLINE trier #-}
trier :: Uses Tryable fs m => Attribute Tryable fs m
trier = Tryable 0 $ \fs ->
    let Tryable i k = view fs
    in pure $ fs .= Tryable (succ i) k

-- | Attribute/Symbol Symmetry

instance Symmetry Tryable Trying where
    symmetry use (Tryable i k) (FreshScope ik) = use k (ik i)

-- | Local Scoping Construct + Substitution

{-# INLINE tries #-}
tries :: Is Trying fs m => (Try a fs m -> Pattern fs m (Maybe a)) -> Pattern fs m (Maybe a)
tries f = do
    scope <- self (FreshScope id)
    transform scope $ f Try
      { success = \a -> self (Success scope a)
      , failure = self (Failure scope)
      }
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
