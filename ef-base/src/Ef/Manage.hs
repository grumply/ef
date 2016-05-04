module Ef.Manage
    ( manage
    , Manager(..)
    , Token
    , Manage
    ) where

import Ef

import Control.Arrow
import Data.Either
import Unsafe.Coerce

newtype Token a = Token Int

data Manage k where
    Manage :: Int -> k -> k -> Manage k

    FreshSelf :: (Int -> k) -> Manage k

    Allocate :: Int -> Narrative self super a
                    -> (a -> Narrative self super ())
                    -> ((a,Token a) -> k)
                    -> Manage k

    Register :: Int -> Token a
                    -> Narrative self super ()
                    -> k
                    -> Manage k

    Unregister :: Int -> Token a -> k -> Manage k

    Deallocate :: Token a -> k -> Manage k

    Finish :: Int -> a -> Manage k

instance Ma Manage Manage where
    -- needed to let Deallocation cross manager scopes
    ma use (Manage _ _ k) (Deallocate _ k') = use k k'
    ma use (Manage i k _) (FreshSelf ik) = use k (ik i)

manages :: (Monad super, '[Manage] .> traits)
        => Trait Manage traits super
manages = Manage 0 pure $ \fs ->
    let Manage i non me = view fs
        i' = succ i
    in i' `seq` pure $ fs .= Manage i' non me
{-# INLINE manages #-}

data Manager self super =
    Manager
        { -- | allocate a resource with a given cleanup method to be performed
          -- when the managed scope returns or when triggered via
          -- deallocate/unregister.
          allocate
              :: forall resource.
                 Narrative self super resource
              -> (resource -> Narrative self super ())
              -> Narrative self super (resource,Token resource)

          -- | deallocate a resource by invoking the `Token`'s registered
          -- actions across all nested managed scopes for which the
          -- resource is registered.
        , deallocate
              :: forall resource. Token resource -> Narrative self super ()

          -- | register a resource with this manager with a given cleanup action
          -- to be performed when deallocate is called or the managed scope
          -- returns. Resources may have multiple actions registered.
        , register
              :: forall resource.
                 Token resource
              -> Narrative self super ()
              -> Narrative self super ()

          -- | unregister a resource within this manager's context without
          -- calling the associated cleanup actions.
        , unregister
              :: forall resource.
                 Token resource
              -> Narrative self super ()
        }

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
manage :: ('[Manage] :> self, Monad super)
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
{-# INLINE manage #-}


rewrite :: forall self super result. ('[Manage] :> self, Monad super)
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
{-# INLINE rewrite #-}
