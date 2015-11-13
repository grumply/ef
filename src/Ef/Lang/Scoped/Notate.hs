{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{- | This module implements a simple scoped Notating interface in the API style of
     mtl's Control.Monad.Notating.
-}
module Ef.Lang.Scoped.Notate
   ( Notating, notates
   , Notatable, notator
   , Notes, tell, listen, listens
   ) where

import Ef.Core

import Data.Monoid

import Unsafe.Coerce

-- | Symbols

data Notating k
    = FreshScope (Int -> k)
    | forall a. Tell Int a
    | forall fs m a. Listen Int (Pattern fs m a)

-- | Symbol Module

data Notes w fs m = Notes
    { tell :: w -> Pattern fs m ()
    , listen :: forall a. Pattern fs m a -> Pattern fs m (w,a)
    }

-- | Attribute

data Notatable k = Notatable Int k

-- | Attribute Construct

{-# INLINE notator #-}
notator :: Uses Notatable fs m => Attribute Notatable fs m
notator = Notatable 0 $ \fs ->
    let Notatable i k = view fs
        i' = succ i
    in i' `seq` pure (fs .= Notatable i' k)

-- | Symbol/Attribute Symmetry

instance Symmetry Notatable Notating where
    symmetry use (Notatable i k) (FreshScope ik) = use k (ik i)

{-# INLINE notates #-}
notates :: forall fs m w r. (Is Notating fs m,Monoid w)
       => (Notes w fs m -> Pattern fs m r) -> Pattern fs m (w,r)
notates f = do
    scope <- self (FreshScope id)
    transform scope mempty $ f Notes
        { tell = \w -> self (Tell scope w)
        , listen = \p -> self (Listen scope p)
        }
  where
    transform scope = go where
        go w = go' where
            go' p = case p of
                Step sym bp -> case prj sym of
                    Just x  -> case x of
                        Tell i w' ->
                            if i == scope
                            then go (w <> (unsafeCoerce w')) (bp (unsafeCoerce ()))
                            else Step sym (\b -> go' (bp b))
                        Listen i p' ->
                            if i == scope
                            then do
                              ~(w',a) <- go mempty (unsafeCoerce p')
                              go (w <> w') (bp (unsafeCoerce (w',a)))
                            else Step sym (\b -> go' (bp b))
                        _ -> Step sym (\b -> go' (bp b))
                    _ -> Step sym (\b -> go' (bp b))
                M m -> M (fmap go' m)
                Pure r -> Pure (w,r)

-- | Extended API

{-# INLINE listens #-}
listens :: Monad m => Notes w fs m -> (w -> b) -> Pattern fs m a -> Pattern fs m (b,a)
listens Notes{..} f m = do
    ~(w, a) <- listen m
    return (f w,a)
