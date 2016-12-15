module Ef.Manage (manage, manages, Manager(..), Token, Manage)
       where

import Ef
import Control.Arrow
import Control.Lens (view, set)
import Data.Either
import Unsafe.Coerce

newtype Token a = Token Int

data Manage k where
  Manage :: Int -> k -> k -> Manage k
  FreshSelf :: (Int -> k) -> Manage k
  Allocate ::
    Int ->
      Code ms c a ->
        (a -> Code ms c ()) -> ((a, Token a) -> k) -> Manage k
  Register ::
    Int -> Token a -> Code ms c () -> k -> Manage k
  Unregister :: Int -> Token a -> k -> Manage k
  Deallocate :: Token a -> k -> Manage k
  Finish :: Int -> a -> Manage k

instance Delta Manage Manage where
    -- needed to let Deallocation cross manager scopes
    delta eval (Manage _ _ k) (Deallocate _ k') = eval k k'
    delta eval (Manage i k _) (FreshSelf ik) = eval k (ik i)

manages :: (Monad c, '[Manage] <. ts) => Manage (Action ts c)
manages = Manage 0 pure $ \o ->
  let Module (Manage i non me) o = o
      !i' = succ i
  in pure $ Module (Manage i' non me) o

{-# INLINE manages #-}

data Manager self super = Manager
    {
      -- | allocate a resource with a given cleanup method to be performed
      -- when the managed scope returns or when triggered via
      -- deallocate/unregister.
      allocate :: forall resource. Code ms c resource -> (resource -> Code ms c ()) -> Code ms c (resource, Token resource)
    ,
      -- | deallocate a resource by invoking the `Token`'s registered
      -- actions across all nested managed scopes for which the
      -- resource is registered.
      deallocate :: forall resource. Token resource -> Code ms c ()
    ,
      -- | register a resource with this manager with a given cleanup action
      -- to be performed when deallocate is called or the managed scope
      -- returns. Resources may have multiple actions registered.
      register :: forall resource. Token resource -> Code ms c () -> Code ms c ()
    ,
      -- | unregister a resource within this manager's context without
      -- calling the associated cleanup actions.
      unregister :: forall resource. Token resource -> Code ms c ()
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
manage :: ('[Manage] <: ms, Monad c) => (Manager ms c -> Code ms c r) -> Code ms c r
manage f = do
  scope <- Send (FreshSelf id)
  rewrite scope [] $
      (f Manager
          { allocate = \create onEnd -> Send (Allocate scope create onEnd id)
          , deallocate = \token -> Send (Deallocate token ())
          , register = \token onEnd -> Send (Register scope token onEnd ())
          , unregister = \token -> Send (Unregister scope token ())
          }
      ) >>= (Send . Finish scope)

{-# INLINE manage #-}

rewrite :: forall ms c r. ('[Manage] <: ms, Monad c) => Int -> [(Int, Code ms c ())] -> Code ms c r -> Code ms c r
rewrite rewriteSelf = withStore
  where
    withStore store = transform id go
      where
        cleanup result = cleanup'
          where
            cleanup' [] = Return result
            cleanup' ((_,x):xs) = do
                () <- x
                cleanup' xs
        go
            :: Messages ms x
            -> Code ms c r
        go message =
            let check currentSelf selfd =
                    if currentSelf == rewriteSelf
                        then selfd
                        else ignore
                ignore = Do (fmap (transform id go) message)
            in case prj message of
                   Just x ->
                       case x of
                           Finish scope result ->
                               check
                                   scope
                                   (cleanup (unsafeCoerce result) store)
                           Allocate currentScope create finalize _ ->
                               check currentScope $
                               do n <- self (FreshSelf id)
                                  a <- unsafeCoerce create
                                  let t = Token n
                                      result = unsafeCoerce (a, t)
                                      cleanup = unsafeCoerce (finalize a)
                                      newStore = (n, cleanup) : store
                                      continue = k result
                                  withStore newStore continue
                           Deallocate (Token t) _ ->
                               let extract =
                                       compose . partitionEithers . map match
                                   compose = second sequence_
                                   match (storedToken,cleanupAction)
                                     | t == storedToken = Right cleanupAction
                                     | otherwise =
                                         Left (storedToken, cleanupAction)
                                   (newStore,cleanup) = extract store
                                   continue b = cleanup >> k b
                               in Say message (withStore newStore . continue)
                           Register currentScope (Token t) finalize _ ->
                               check currentScope $
                               let result = unsafeCoerce ()
                                   cleanup = unsafeCoerce (t, finalize)
                                   newStore = cleanup : store
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
