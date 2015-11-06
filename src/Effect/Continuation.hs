{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Continuation
  (Continuation,enter
  ,Continuations,continuations
  ) where

import Mop.Core
import Unsafe.Coerce

data Continuation k
    = FreshScope (Int -> k)
    | forall a. Continuation Int a
data Continuations k = Continuations Int k

{-# INLINE enter #-}
-- use: enter $ \exit -> do { .. ; }
enter :: Has Continuation fs m => ((forall b. a -> Plan fs m b) -> Plan fs m a) -> Plan fs m a
enter x = do
    scope <- freshScope
    transform scope $ x (\a -> self (Continuation scope a))
  where
    transform scope = go where
        go p = case p of
            Step sym bp -> case prj sym of
                Just x -> case x of
                    Continuation i a ->
                        if i == scope
                        then return (unsafeCoerce a)
                        else Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                _ -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r

{-# INLINE freshScope #-}
freshScope :: Has Continuation fs m => Plan fs m Int
freshScope = self (FreshScope id)

{-# INLINE continuations #-}
continuations :: Uses Continuations gs m => Attribute Continuations gs m
continuations = Continuations 0 $ \fs ->
    let Continuations i k = view fs
        i' = succ i
    in i' `seq` pure $ fs .= Continuations i' k

instance Pair Continuations Continuation where
    pair p (Continuations i k) (FreshScope ik) = p k (ik i)
    pair p _ (Continuation _ _) = error "Unscoped continuation."
