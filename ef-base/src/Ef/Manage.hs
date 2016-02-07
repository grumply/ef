{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Manage
    ( -- * Scoping construct
      manage
      -- * Scoped methods
    , Manager(..)
      -- * Resource Token
    , Token
    ) where

import Ef

import qualified Ef.Manage.Methods as Methods
import Ef.Manage.Messages
import qualified Ef.Manage.Messages as Messages

import Control.Arrow
import Data.Either
import Unsafe.Coerce


instance Ma Methods.Manage Messages.Manage where
    -- needed to let Deallocation cross manager scopes
    ma use (Methods.Manage _ _ k) (Deallocate _ k') = use k k'
    ma use (Methods.Manage i k _) (FreshSelf ik) = use k (ik i)


-- | managed creates a scoped construct to interface with
-- the manager.
--
-- Simple Example:
--
--    > managed $ \Manager{..} -> do
--    >     (handle,fileToken) <- allocate (io $ openFile "./file.tmp") hClose
--    >     result <- withHandle handle something
--    >     return result -- hClose will happen before the return
--
-- Resources are released in LIFO order as expected. Tokens may be registered
-- with other managers with an action to perform upon manager scope completion.
-- Tokens may be unregistered to prevent cleanup actions in individual managers.
-- Tokens may be deallocated which forces registered cleanup actions to be
-- performed in all managers for which that token is registered in LIFO order
-- of the nesting scopes.
manage :: Knows Manage self super
        => (Manager self super -> Narrative self super result)
        -> Narrative self super result
manage f = do
    scope <- self (FreshSelf id)
    rewrite scope [] $
        (f Manager
            { allocate   = \create onEnd -> self (Allocate scope create onEnd id)
            , deallocate = \token -> self (Deallocate token ())
            , register   = \token onEnd -> self (Register scope token onEnd ())
            , unregister = \token -> self (Unregister scope token ())
            }
        ) >>= (self . Finish scope)


rewrite :: forall self super result.
           Knows Manage self super
        => Int
        -> [(Int,Narrative self super ())]
        -> Narrative self super result
        -> Narrative self super result
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
                                           compose = second sequence_
                                           match (storedToken,cleanupAction)
                                             | t == storedToken = Right cleanupAction
                                             | otherwise = Left (storedToken,cleanupAction)
                                           (newStore,cleanup) = extract store
                                           continue b = cleanup >> k b

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
{-# INLINE manage #-}
