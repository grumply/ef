{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
        :: Int -> k -> k -> Manageable k



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
  where

    rewrite scope =
        withStore
      where

        withStore store = go
          where

            go (Step sym bp) =
                let
                  check i scoped =
                      if i == scope then
                          scoped
                      else
                          ignore

                  ignore =
                      Step sym (go . bp)

                in
                  case prj sym of

                      Just x ->
                          case x of

                              Allocate i create oE _ ->
                                  check i $
                                      do
                                        n <- self (FreshScope id)
                                        a <- unsafeCoerce create
                                        let
                                          t =
                                              Token n

                                          result =
                                              unsafeCoerce (a,t)

                                          cleanup =
                                              unsafeCoerce (oE a)

                                          newStore =
                                              (n,cleanup):store

                                          continue =
                                              bp result

                                        withStore newStore continue

                              Deallocate (Token t) _ ->
                                  let
                                    (newStore,cleanup) =
                                        extract t store

                                    continue b =
                                        do
                                          cleanup
                                          bp b

                                  in
                                    Step sym (withStore newStore . continue)

                              Register i (Token t) p' _ ->
                                  check i $
                                      let
                                        result =
                                            unsafeCoerce ()

                                        cleanup =
                                            unsafeCoerce (t,p')

                                        newStore =
                                            cleanup:store

                                        continue =
                                            bp result

                                      in
                                        withStore newStore continue

                              Unregister i (Token t) _ ->
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
                                    check i $
                                      withStore newStore continue

                              _ ->
                                  ignore

                      Nothing ->
                          ignore

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

    extract n =
        finish . partitionEithers . map go
      where

        finish (xs,rs) =
            let
              amass =
                  foldr (>>) (return ()) rs

            in
              (xs,amass)

        go (t,x)

            | t == n = Right x

            | otherwise = Left (t,x)



-- | Inlines

{-# INLINE manager #-}
{-# INLINE manages #-}
