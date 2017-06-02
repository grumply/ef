module Ef.Manage (manage, manages, Manager(..), Token, Manage)
       where

import Ef
import Data.Bifunctor
import Data.Either
import Unsafe.Coerce

newtype Token = Token Int

data Manage k where
  Manage :: Int -> k -> k -> Manage k
  FreshSelf :: (Int -> k) -> Manage k
  Allocate ::
    Int ->
      Ef ms c a ->
        (a -> Ef ms c ()) -> ((a, Token) -> k) -> Manage k
  Register ::
    Int -> Token -> Ef ms c () -> k -> Manage k
  Unregister :: Int -> Token -> k -> Manage k
  Deallocate :: Token -> k -> Manage k
  Finish :: Int -> a -> Manage k

instance Functor Manage where
  fmap f (Manage i k k') = Manage i (f k) (f k')
  fmap f (FreshSelf ik) = FreshSelf (fmap f ik)
  fmap f (Allocate i ca acu atak) = Allocate i ca acu (fmap f atak)
  fmap f (Register i t c k) = Register i t c (f k)
  fmap f (Unregister i t k) = Unregister i t (f k)
  fmap f (Deallocate t k) = Deallocate t (f k)
  fmap f (Finish i a) = Finish i a

instance Delta Manage Manage where
    -- needed to let Deallocation cross manager scopes
    delta eval (Manage _ _ k) (Deallocate _ k') = eval k k'
    delta eval (Manage i k _) (FreshSelf ik) = eval k (ik i)

manages :: (Monad c, '[Manage] <. ts) => Manage (Action ts c)
manages = flip (Manage 0) pure $ \o ->
  let Module (Manage i non me) _ = o
      !i' = succ i
  in pure $ Module (Manage i' non me) o

{-# INLINE manages #-}

data Manager ms c = Manager
    {
      -- | allocate a resource with a given cleanup method to be performed
      -- when the managed scope returns or when triggered via
      -- deallocate/unregister.
      allocate :: forall resource. Ef ms c resource -> (resource -> Ef ms c ()) -> Ef ms c (resource, Token)
    ,
      -- | deallocate a resource by invoking the `Token`'s registered
      -- actions across all nested managed scopes for which the
      -- resource is registered.
      deallocate :: Token -> Ef ms c ()
    ,
      -- | register a resource with this manager with a given cleanup action
      -- to be performed when deallocate is called or the managed scope
      -- returns. Resources may have multiple actions registered.
      register :: Token -> Ef ms c () -> Ef ms c ()
    ,
      -- | unregister a resource within this manager's context without
      -- calling the associated cleanup actions.
      unregister :: Token -> Ef ms c ()
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
manage :: ('[Manage] <: ms, Monad c) => (Manager ms c -> Ef ms c r) -> Ef ms c r
manage f = do
  scope <- Send (FreshSelf Return)
  rewrite scope [] $
      (f Manager
          { allocate = \create onEnd -> Send (Allocate scope create onEnd Return)
          , deallocate = \token -> Send (Deallocate token (Return ()))
          , register = \token onEnd -> Send (Register scope token onEnd (Return ()))
          , unregister = \token -> Send (Unregister scope token (Return ()))
          }
      ) >>= (Send . Finish scope)

{-# INLINE manage #-}

rewrite :: forall ms c r. ('[Manage] <: ms, Monad c) => Int -> [(Int, Ef ms c ())] -> Ef ms c r -> Ef ms c r
rewrite rewriteSelf = withStore
  where
    withStore store = go
      where
        cleanup result = cleanup'
          where
            cleanup' [] = Return result
            cleanup' ((_,x):xs) = do
                () <- x
                cleanup' xs
        go (Return r) = Return r
        go (Lift c) = Lift c
        go (Do message) =
            let check currentSelf selfd =
                    if currentSelf == rewriteSelf
                        then selfd
                        else ignore
                ignore = Do (fmap go message)
            in case prj message of
                   Just x ->
                       case x of
                           Finish scope result ->
                               check scope (cleanup (unsafeCoerce result) store)
                           Allocate currentScope create finalize k ->
                               check currentScope $
                               do n <- Send (FreshSelf Return)
                                  a <- unsafeCoerce create
                                  let t = Token n
                                      result = unsafeCoerce (a, t)
                                      cleanup = unsafeCoerce (finalize a)
                                      newStore = (n, cleanup) : store
                                      continue = k result
                                  withStore newStore continue
                           Deallocate (Token t) k ->
                               let extract =
                                       compose . partitionEithers . map match
                                   compose = second sequence_
                                   match (storedToken,cleanupAction)
                                     | t == storedToken = Right cleanupAction
                                     | otherwise =
                                         Left (storedToken, cleanupAction)
                                   (newStore,cleanup) = extract store
                                   continue = cleanup >> k
                               in withStore newStore continue
                           Register currentScope (Token t) finalize k ->
                               check currentScope $
                               let result = unsafeCoerce ()
                                   cleanup = unsafeCoerce (t, finalize)
                                   newStore = cleanup : store
                               in withStore newStore k
                           Unregister currentScope (Token t) k ->
                               let mismatch = (/= t) . fst
                                   newStore = filter mismatch store
                                   result = unsafeCoerce ()
                               in check currentScope $ withStore newStore k
                           _ -> ignore
                   _ -> ignore

{-# INLINE rewrite #-}
