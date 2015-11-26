{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.Manage
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

import Ef.Core

import Control.Arrow
import Data.Binary
import Data.Either
import Unsafe.Coerce



newtype Token a = Token Int



data Managing k
  where

    FreshScope
        :: (Int -> k)
        -> Managing k

    -- this looks like something out of 'Notions of Computation as Monoids'
    Allocate
        :: Int
        -> Pattern fs m a
        -> (a -> Pattern fs m ())
        -> ((a,Token a) -> k)
        -> Managing k

    Register
        :: Int
        -> Token a
        -> Pattern fs m ()
        -> k
        -> Managing k

    Unregister
        :: Int
        -> Token a
        -> k
        -> Managing k

    Deallocate
        :: Token a
        -> k
        -> Managing k



data Manageable k
  where

    Manageable
        :: Int
        -> k
        -> k
        -> Manageable k



instance Uses Manageable gs m
    => Binary (Attribute Manageable gs m)
  where

    get =
        do
          scope <- get
          let
            Manageable _ k k' = manager

          return (Manageable scope k k')

    put (Manageable scope _ _) =
        put scope



manager :: Uses Manageable fs m => Attribute Manageable fs m
manager =
    Manageable 0 return $ \fs ->
        let
          Manageable i non me =
              view fs

          i' =
              succ i

        in
          i' `seq` pure $ fs .=
              Manageable i' non me



instance Witnessing Manageable Managing
  where

    witness use (Manageable _ _ k) (Deallocate _ k') =
        use k k'

    witness use (Manageable i k _) (FreshScope ik) =
        use k (ik i)



data Manage fs m =
    Manage
        {
          allocate
              :: forall a.
                 Pattern fs m a
              -> (a -> Pattern fs m ())
              -> Pattern fs m (a,Token a)

        , deallocate
              :: forall a.
                 Token a
              -> Pattern fs m ()

        , register
              :: forall a.
                 Token a
              -> Pattern fs m ()
              -> Pattern fs m ()

        , unregister
              :: forall a.
                 Token a
              -> Pattern fs m ()
        }



manages
    :: forall fs m r.
       Is Managing fs m
    => (    Manage fs m
         -> Pattern fs m r
       )
    -> Pattern fs m r

manages f = do
    scope <- self (FreshScope id)
    rewrite scope [] $ f
        Manage
            {
              allocate =
                  \create onEnd ->
                      self (Allocate scope create onEnd id)

            , deallocate =
                  \token ->
                      self (Deallocate token ())

            , register =
                  \token onEnd ->
                      self (Register scope token onEnd ())

            , unregister =
                  \token ->
                      self (Unregister scope token ())
            }



rewrite rewriteScope =
    withStore
  where

    withStore store = go
      where

        go (Fail e) =
            Fail e

        go (M m) =
            M (fmap go m)

        go (Pure r) =
            case store of

                [] ->
                    Pure r

                xs ->
                    let
                      newStore =
                          tail xs

                      cleanup =
                          snd (head xs)

                      continue =
                          do
                            () <- cleanup
                            return r

                    in
                      withStore newStore continue

        go (Step sym bp) =
            let
              check currentScope scoped =
                  if currentScope == rewriteScope then
                      scoped
                  else
                      ignore

              ignore =
                  Step sym (go . bp)

            in
              case prj sym of

                  Just x ->
                      case x of

                          Allocate currentScope create finalize _ ->
                              check currentScope $
                                  do
                                    n <- self (FreshScope id)
                                    a <- unsafeCoerce create
                                    let
                                      t =
                                          Token n

                                      result =
                                          unsafeCoerce (a,t)

                                      cleanup =
                                          unsafeCoerce (finalize a)

                                      newStore =
                                          (n,cleanup):store

                                      continue =
                                          bp result

                                    withStore newStore continue

                          Deallocate (Token t) _ ->
                              let
                                extract =
                                    compose . partitionEithers . map match

                                compose =
                                    let
                                      amass =
                                          foldr (>>) (return ())

                                    in
                                      second amass

                                match (storedToken,cleanupAction)

                                    | t == storedToken =
                                          Right cleanupAction

                                    | otherwise =
                                          Left (storedToken,cleanupAction)

                                (newStore,cleanup) =
                                    extract store

                                continue b =
                                    do
                                      cleanup
                                      bp b

                              in
                                Step sym (withStore newStore . continue)

                          Register currentScope (Token t) finalize _ ->
                              check currentScope $
                                  let
                                    result =
                                        unsafeCoerce ()

                                    cleanup =
                                        unsafeCoerce (t,finalize)

                                    newStore =
                                        cleanup:store

                                    continue =
                                        bp result

                                  in
                                    withStore newStore continue

                          Unregister currentScope (Token t) _ ->
                              let
                                mismatch =
                                    (/= t) . fst

                                newStore =
                                    filter mismatch store

                                result =
                                    unsafeCoerce ()

                                continue =
                                    bp result

                              in
                                check currentScope $
                                    withStore newStore continue

                          _ ->
                              ignore

                  _ ->
                      ignore



-- | Inlines

{-# INLINE rewrite #-}
{-# INLINE manager #-}
{-# INLINE manages #-}
