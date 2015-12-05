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
        -> Pattern scope parent a
        -> (a -> Pattern scope parent ())
        -> ((a,Token a) -> k)
        -> Managing k

    Register
        :: Int
        -> Token a
        -> Pattern scope parent ()
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



instance Uses Manageable attrs parent
    => Binary (Attribute Manageable attrs parent)
  where

    get =
        return manager



    put _ =
        pure ()



manager
    :: Uses Manageable attrs parent
    => Attribute Manageable attrs parent

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



data Manage scope parent =
    Manage
        {
          allocate
              :: forall resource.
                 Pattern scope parent resource
              -> (resource -> Pattern scope parent ())
              -> Pattern scope parent (resource,Token resource)

        , deallocate
              :: forall resource.
                 Token resource
              -> Pattern scope parent ()

        , register
              :: forall resource.
                 Token resource
              -> Pattern scope parent ()
              -> Pattern scope parent ()

        , unregister
              :: forall resource.
                 Token resource
              -> Pattern scope parent ()
        }



manages
    :: forall scope parent result.
       Is Managing scope parent
    => (    Manage scope parent
         -> Pattern scope parent result
       )
    -> Pattern scope parent result

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

        go (Super m) =
            Super (fmap go m)

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

        go (Send sym bp) =
            let
              check currentScope scoped =
                  if currentScope == rewriteScope then
                      scoped
                  else
                      ignore

              ignore =
                  Send sym (go . bp)

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
                                Send sym (withStore newStore . continue)

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
