{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Manage.Messages where


import Ef.Narrative



newtype Token a = Token Int



data Manage k where

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



-- | Interface for managing resources. The only way to create a Managed is with
-- `Ef.Manage.managed`. As with all scoping constructs, never return a `Managed`
-- from its root scope.
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
