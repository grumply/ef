{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Manage
    ( Managing
    , Manageable
    , manager
    , manages
    , Manage
    , Token
    , deallocate
    , allocate
    , register
    , unregister
    ) where

import Ef

import Control.Arrow
import Data.Either
import Unsafe.Coerce



newtype Token a = Token Int


data Managing k where

    FreshSelf :: (Int -> k) -> Managing k

    Allocate :: Int -> Narrative self super a
                    -> (a -> Narrative self super ())
                    -> ((a,Token a) -> k)
                    -> Managing k

    Register :: Int -> Token a
                    -> Narrative self super ()
                    -> k
                    -> Managing k

    Unregister :: Int -> Token a -> k -> Managing k

    Deallocate :: Token a -> k -> Managing k

    Finish :: Int -> a -> Managing k


data Manageable k where

    Manageable :: Int -> k -> k -> Manageable k


manager =
    Manageable 0 return $ \fs ->
        let Manageable i non me = view fs
            i' = succ i
        in i' `seq` pure $ fs .= Manageable i' non me


instance Ma Manageable Managing where
    -- needs to slip through
    ma use (Manageable _ _ k) (Deallocate _ k') = use k k'
    ma use (Manageable i k _) (FreshSelf ik) = use k (ik i)


data Manage self super =
    Manage
        {
          allocate
              :: forall resource.
                 Narrative self super resource
              -> (resource -> Narrative self super ())
              -> Narrative self super (resource,Token resource)

        , deallocate
              :: forall resource. Token resource -> Narrative self super ()

        , register
              :: forall resource.
                 Token resource
              -> Narrative self super ()
              -> Narrative self super ()

        , unregister
              :: forall resource.
                 Token resource
              -> Narrative self super ()
        }


manages f = do
    scope <- self (FreshSelf id)
    rewrite scope [] $
        (f Manage
            { allocate   = \create onEnd -> self (Allocate scope create onEnd id)
            , deallocate = \token -> self (Deallocate token ())
            , register   = \token onEnd -> self (Register scope token onEnd ())
            , unregister = \token -> self (Unregister scope token ())
            }
        ) >>= (self . Finish scope)


rewrite :: forall self super result.
           (Can Managing self, Monad super)
        => Int -> [(Int,Narrative self super ())] -> Narrative self super result -> Narrative self super result
rewrite rewriteSelf =
    withStore
    where

        withStore store =
            transform go
            where

                cleanup result = cleanup'
                    where
                        cleanup' [] = Return result
                        cleanup' ((_,x):xs) = do
                            () <- x
                            cleanup' xs

                go :: Messages self x
                   -> (x -> Narrative self super result)
                   -> Narrative self super result
                go message k =
                    let check currentSelf selfd =
                            if currentSelf == rewriteSelf then selfd else ignore
                        ignore = Say message (withStore store . k)
                    in case prj message of
                           Just x ->
                               case x of

                                   Finish scope result ->
                                       check scope (cleanup (unsafeCoerce result) store)

                                   Allocate currentScope create finalize _ ->
                                       check currentScope $
                                           do
                                               n <- self (FreshSelf id)
                                               a <- unsafeCoerce create
                                               let t = Token n
                                                   result = unsafeCoerce (a,t)
                                                   cleanup = unsafeCoerce (finalize a)
                                                   newStore = (n,cleanup):store
                                                   continue = k result

                                               withStore newStore continue

                                   Deallocate (Token t) _ ->
                                       let extract = compose . partitionEithers . map match
                                           compose =
                                               let amass = foldr (>>) (return ())
                                               in second amass
                                           match (storedToken,cleanupAction)
                                             | t == storedToken = Right cleanupAction
                                             | otherwise = Left (storedToken,cleanupAction)
                                           (newStore,cleanup) = extract store
                                           continue b = do
                                               cleanup
                                               k b

                                       in Say message (withStore newStore . continue)

                                   Register currentScope (Token t) finalize _ ->
                                       check currentScope $
                                           let result = unsafeCoerce ()
                                               cleanup = unsafeCoerce (t,finalize)
                                               newStore = cleanup:store
                                               continue = k result

                                           in withStore newStore continue

                                   Unregister currentScope (Token t) _ ->
                                       let mismatch = (/= t) . fst
                                           newStore = filter mismatch store
                                           result = unsafeCoerce ()
                                           continue = k result

                                       in check currentScope $
                                              withStore newStore continue

                                   _ -> ignore
                           _ -> ignore

-- | Inlines

{-# INLINE rewrite #-}
{-# INLINE manager #-}
{-# INLINE manages #-}
