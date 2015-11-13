{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Scoped.Vary
    ( Varying, varies
    , Variable, varier
    , Vary(..)
    ) where

import Ef.Core

import Unsafe.Coerce

-- | Symbols

data Eagerness = Strict | Lazy deriving Eq

data Varying k
    = FreshScope (Int -> k)
    | forall a. Modify Int Eagerness (a -> a) (a -> k)

-- | Symbol Module

data Vary fs m st = Vary
  { modify :: (st -> st) -> Pattern fs m ()
  , modify' :: (st -> st) -> Pattern fs m ()
  , get :: Pattern fs m st
  , gets :: forall a. (st -> a) -> Pattern fs m a
  , put :: st -> Pattern fs m ()
  , puts :: forall a. (a -> st) -> a -> Pattern fs m ()
  , swap :: st -> Pattern fs m st
  }

-- | Attribute

data Variable k = Variable Int k

-- | Attribute Construct

{-# INLINE varier #-}
varier :: Uses Variable fs m => Attribute Variable fs m
varier = Variable 0 $ \fs ->
    let Variable i k = view fs
        i' = succ i
    in i' `seq` pure (fs .= Variable i' k)

-- | Attribute/Symbol Symmetry

instance Symmetry Variable Varying where
    symmetry use (Variable i k) (FreshScope ik) = use k (ik i)

-- | Local Scoping Construct + Substitution

{-# INLINE varies #-}
varies :: forall fs m st r. Is Varying fs m
      => st -> (Vary fs m st -> Pattern fs m r) -> Pattern fs m (st,r)
varies st0 f0 = do
    scope <- self (FreshScope id)
    transform scope st0 $ f0 Vary
      { modify = \f -> self (Modify scope Lazy f (const ()))
      , modify' = \f -> self (Modify scope Strict f (const ()))
      , get = self (Modify scope Lazy id id)
      , gets = \f -> self (Modify scope Lazy id f)
      , put = \st -> self (Modify scope Lazy (const st) (const ()))
      , puts = \f a -> self (Modify scope Lazy (const (f a)) (const ()))
      , swap = \a -> self (Modify scope Lazy (const a) id)
      }
  where
    transform scope = go where
        go st = go' where
            go' p = case p of
                Step sym bp -> case prj sym of
                    Just x  -> case x of
                        Modify i sl f g ->
                            if i == scope
                            then
                              if sl == Strict
                              then let st' = unsafeCoerce f st
                                    in st' `seq` go st' (bp (unsafeCoerce g st))
                              else go (unsafeCoerce f st) (bp (unsafeCoerce g st))
                            else Step sym (\b -> go' (bp b))
                        _ -> Step sym (\b -> go' (bp b))
                    Nothing -> Step sym (\b -> go' (bp b))
                M m -> M (fmap go' m)
                Pure r -> Pure (st,r)
