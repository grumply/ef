{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{- | This module implements a feature-paired interface with Writer with the
     addition of a re/configurable combining function. That is, this is a
     generalization of Writer with the Monoid constraint removed.

     Naming is slightly changed for convenience.
       'tell' becomes 'log'
       'listen' becomes 'eavesdrop'
       'listens' becomes 'intercept'
-}
module Lang.Scoped.Log
    ( Logging, logger
    , Loggable, logs
    , Log(..), intercept
    ) where

import Ef.Core

import Data.Monoid

import Unsafe.Coerce

import Prelude hiding (log)

-- | Symbol

data Logging k
    = FreshScope (Int -> k)
    | forall a. Logs Int a
    | forall fs m a w. Eavesdrop Int w (Pattern fs m a)
    | forall a w. Reconfigure Int (a -> w -> w)

-- | Symbol Module

data Log fs m a w = Log
    { log :: a -> Pattern fs m ()
    , eavesdrop :: forall r. w -> Pattern fs m r -> Pattern fs m (w,r)
    , reconfigure :: (a -> w -> w) -> Pattern fs m ()
    }

-- | Attribute

data Loggable k = Loggable Int k

-- | Attribute Construct

{-# INLINE logger #-}
logger :: Uses Loggable fs m => Attribute Loggable fs m
logger = Loggable 0 $ \fs ->
    let Loggable i k = view fs
        i' = succ i
    in i' `seq` pure (fs .= Loggable i' k)

-- | Symbol/Attribute Symmetry

instance Symmetry Loggable Logging where
    symmetry use (Loggable i k) (FreshScope ik) = use k (ik i)

-- | Local Scoping Construct + Substitution

{-# INLINE logs #-}
logs :: forall fs m a w r. (Is Logging fs m,Monoid w)
       => (a -> w -> w) -> w -> (Log fs m a w -> Pattern fs m r) -> Pattern fs m (w,r)
logs c0 w0 f = do
    scope <- self (FreshScope id)
    transform scope c0 w0 $ f Log
        { log = \w -> self (Logs scope w)
        , eavesdrop = \w' p -> self (Eavesdrop scope w' p)
        , reconfigure = \c' -> self (Reconfigure scope c')
        }
  where
    transform scope = go where
        go c = go' where
            go' w = go'' where
                go'' p = case p of
                    Step sym bp -> case prj sym of
                        Just x  -> case x of
                            Logs i a ->
                                if i == scope
                                then go' (c (unsafeCoerce a) w)
                                         (bp (unsafeCoerce ()))
                                else Step sym (\b -> go'' (bp b))
                            Eavesdrop i w' p' ->
                                if i == scope
                                then do
                                  ~(w'',r) <- go' (unsafeCoerce w') (unsafeCoerce p')
                                  go' (w <> w'') (bp (unsafeCoerce (w'',r)))
                                else Step sym (\b -> go'' (bp b))
                            Reconfigure i c' ->
                                if i == scope
                                then go (unsafeCoerce c') (unsafeCoerce w) (bp (unsafeCoerce ()))
                                else Step sym (\b -> go'' (bp b))
                            _ -> Step sym (\b -> go'' (bp b))
                        _ -> Step sym (\b -> go'' (bp b))
                    M m -> M (fmap go'' m)
                    Pure r -> Pure (w,r)

-- | Extended API

{-# INLINE intercept #-}
intercept :: Monad m => Log fs m a w -> (w -> b) -> w -> Pattern fs m r -> Pattern fs m (b,r)
intercept Log{..} f w0 m = do
    ~(w, a) <- eavesdrop w0 m
    return (f w,a)
