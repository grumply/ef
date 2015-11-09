{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Lang.Scoped.Exit
  (Exiting,exits
  ,Exitable,exiter
  ) where

import Mop.Core
import Unsafe.Coerce

-- | Symbol

data Exiting k
    = FreshScope (Int -> k)
    | forall a. Exit Int a

-- | Attribute

data Exitable k = Exitable Int k

-- | Attribute Construct

{-# INLINE exiter #-}
exiter :: Uses Exitable gs m => Attribute Exitable gs m
exiter = Exitable 0 $ \fs ->
    let Exitable i k = view fs
        i' = succ i
    in i' `seq` pure $ fs .= Exitable i' k

-- | Symbol/Attribute Symmetry

instance Symmetry Exitable Exiting where
    symmetry use (Exitable i k) (FreshScope ik) = use k (ik i)

-- | Local Scoping Construct + Symbol Substitution

{-# INLINE exits #-}
-- use: exits $ \exit -> do { .. ; }
exits :: Is Exiting fs m => ((forall b. a -> Plan fs m b) -> Plan fs m a) -> Plan fs m a
exits f = do
    scope <- self (FreshScope id)
    transform scope $ f (\a -> self (Exit scope a))
  where
    transform scope = go where
        go p = case p of
            Step sym bp -> case prj sym of
                Just x -> case x of
                    Exit i a ->
                        if i == scope
                        then return (unsafeCoerce a)
                        else Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                _ -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r
