{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
-}
{-
TODO: Spell out the exact semantics of this module. Notably, the approach
      with deallocate not being removed by a child scope but unregister
      being removed could be confusing.
-}
module Lang.Scoped.Manage
  ( Managing, Manageable, manager, manages
  , ManagingScope
  , Token
  , deallocate, allocate, register, unregister, onEnd
  ) where

import Mop.Core

import Data.Either

import Unsafe.Coerce

-- this module does not have the same sort of asynchronous exception safety
-- as in resourcet, but still admits a nice interface for allocate/deallocate
-- as well as onEnd/register/unregister.

-- I believe the only approach that will fix the asynchronous exception safety
-- issue is attaching transient to the top-level scope by embedding it inside
-- delta. But as soon as you embed it in delta, performance will drop
-- tremendously. Please, if you have any ideas, send them to me at:
-- sean@grump.ly

-- For now, exception safety can be guaranteed through internal use of mio for
-- finalizers. However, this will not permit exception safety /between/
-- executions of finalizers, sadly. Thus, to achieve an acceptable level of
-- exception safety, it is imperative not to conflate resource management
-- with asynchronous exception safety; we must, by current design, be careful
-- when using resources and make thoughtful use of exception handling.


newtype Token a = Token Int

data Managing k
    = FreshScope (Int -> k)

    | forall fs m a . Allocate   Int (Plan fs m a) (a -> Plan fs m ()) ((a,Token a) -> k)

    | forall fs m   . OnEnd      Int           (Plan fs m ()) (Token () -> k)
    | forall fs m a . Register   Int (Token a) (Plan fs m ()) k

    | forall      a . Unregister Int (Token a) k
    | forall      a . Deallocate     (Token a) k

data Manageable k = Manageable Int k k

{-# INLINE manager #-}
manager :: Uses Manageable fs m => Attribute Manageable fs m
manager = Manageable 0 return $ \fs ->
    let Manageable i non me = view fs
        i' = succ i
    in i' `seq` pure (fs .= Manageable i' non me)

instance Symmetry Manageable Managing where
    symmetry use (Manageable _ _ k) (Deallocate _ k') = use k k'
    symmetry use (Manageable i k _) (FreshScope ik)   = use k (ik i)

{-# INLINE freshScope #-}
freshScope :: Is Managing fs m => Plan fs m Int
freshScope = self (FreshScope id)

{-# INLINE deallocate #-}
deallocate :: Is Managing fs m => Token a -> Plan fs m ()
deallocate rsrc = self (Deallocate rsrc ())

data ManagingScope fs m = ManagingScope
    { allocate   :: forall a. Plan fs m a -> (a -> Plan fs m ()) -> Plan fs m (a,Token a)
    , register   :: forall a. Token a -> Plan fs m () -> Plan fs m ()
    , unregister :: forall a. Token a -> Plan fs m ()
    , onEnd      ::           Plan fs m () -> Plan fs m (Token ())
    }

{-# INLINE manages #-}
-- use: manages $ \transient -> do
--        (a,key) <- allocate transient _ _
--        transient&unregister key
--        onEnd transient _
--        deallocate key
manages :: forall fs m r. Is Managing fs m
            => (    ManagingScope fs m
                 -> Plan fs m r
               ) -> Plan fs m r
manages f = do
    scope <- freshScope
    let a create oe = self (Allocate scope create oe id)
        r token oe = self (Register scope token oe ())
        u token = self (Unregister scope token ())
        o oE = self (OnEnd scope oE id)
    transform scope [] $ f $ ManagingScope a r u o
  where
    transform scope = go
      where
        go store = go'
          where
            go' :: Plan fs m r -> Plan fs m r
            go' p = case p of
                Step sym bp -> case prj sym of
                    Just x  -> case x of
                        Allocate i create oE _ ->
                            if i == scope
                            then do
                              n <- freshScope
                              let t = Token n
                              a <- unsafeCoerce create
                              go ((n,unsafeCoerce (oE a)):store)
                                 (bp (unsafeCoerce (a,t)))
                            else Step sym (\b -> go' (bp b))
                        Deallocate (Token t) _ -> case extract t store of
                            Just (store',cleanup) ->
                                Step sym (\b -> go store' (cleanup >> bp b))
                            Nothing -> Step sym (\b -> go' (bp b))
                        Register i (Token t) p' _ ->
                            if i == scope
                            then go (unsafeCoerce (t,p'):store)
                                    (bp (unsafeCoerce ()))
                            else Step sym (\b -> go' (bp b))
                        Unregister i (Token t) _ ->
                            if i == scope
                            then go (filter ((/= t) . fst) store)
                                    (bp (unsafeCoerce ()))
                            else Step sym (\b -> go' (bp b))
                        OnEnd i oE _ -> do
                            if i == scope
                            then do
                                n <- freshScope
                                go (unsafeCoerce (n,oE):store)
                                   (bp (unsafeCoerce (Token n :: Token ())))
                            else Step sym (\b -> go' (bp b))
                        _ -> Step sym (\b -> go' (bp b))
                    Nothing -> Step sym (\b -> go' (bp b))
                M m -> M (fmap go' m)
                Pure r -> case store of
                    [] -> Pure r
                    ((_,x):xs) -> go xs (x >> return r)

    extract n = finish . partitionEithers . map go
      where
        finish (_,[]) = Nothing
        finish (xs,[a]) = Just (xs,a)
        finish (xs,rs) = Just (xs,foldr1 (>>) rs :: Plan fs m ())
        go (t,x) = if t == n then Right x else Left (t,x)
