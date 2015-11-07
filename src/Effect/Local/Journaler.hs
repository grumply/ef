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
module Effect.Local.Journaler
    ( Journaler, journaling
    , Journaling, journal
    , Journal(..), intercept
    ) where

import Mop.Core

import Data.Monoid

import Unsafe.Coerce

import Prelude hiding (log)

data Journaler k
    = FreshScope (Int -> k)
    | forall a. Log Int a
    | forall fs m a w. Eavesdrop Int w (Plan fs m a)
    | forall a w. Reconfigure Int (a -> w -> w)

data Journal fs m a w = Journal
    { log :: a -> Plan fs m ()
    , eavesdrop :: forall r. w -> Plan fs m r -> Plan fs m (w,r)
    , reconfigure :: (a -> w -> w) -> Plan fs m ()
    }

{-# INLINE intercept #-}
intercept :: Monad m => Journal fs m a w -> (w -> b) -> w -> Plan fs m r -> Plan fs m (b,r)
intercept Journal{..} f w0 m = do
    ~(w, a) <- eavesdrop w0 m
    return (f w,a)

{-# INLINE journal #-}
journal :: forall fs m a w r. (Has Journaler fs m,Monoid w)
       => (a -> w -> w) -> w -> (Journal fs m a w -> Plan fs m r) -> Plan fs m (w,r)
journal c0 w0 f = do
    scope <- self (FreshScope id)
    transform scope c0 w0 $ f Journal
        { log = \w -> self (Log scope w)
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
                            Log i a ->
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

data Journaling k = Journaling Int k

{-# INLINE journaling #-}
journaling :: Uses Journaling fs m => Attribute Journaling fs m
journaling = Journaling 0 $ \fs ->
    let Journaling i k = view fs
        i' = succ i
    in i' `seq` pure (fs .= Journaling i' k)

instance Pair Journaling Journaler where
    pair p (Journaling i k) (FreshScope ik) = p k (ik i)
