{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Contract
    ( contract
    , Consideration(..)
    , Breaches(..)
    , Contract(..)
    , Phase(..)
    ) where



import Ef.Core

import Control.Exception
    ( Exception(..)
    , SomeException
    )

import Data.Either



data Phase
  where

    Before
        :: Phase

    During
        :: Phase

    After
        :: Phase

  deriving Show



data Breaches
  where

    Breaches
        :: Phase
        -> [SomeException]
        -> Breaches

  deriving Show
instance Exception Breaches



data Consideration variables result fs m
  where

    Precondition
        :: Pattern fs m ()
        -> Consideration variables result fs m

    Postcondition
        :: (    result
             -> Pattern fs m ()
           )
        -> Consideration variables result fs m

    Invariant
        :: Pattern fs m ()
        -> Consideration variables result fs m




data Contract variables result fs m
  where

    Contract
        :: [Consideration variables result fs m]
        -> Pattern fs m result
        -> Contract variables result fs m



consider
    :: Monad m
    => [Pattern fs m ()]
    -> Pattern fs m [SomeException]

consider considerations =
    do
      results <- sequence (map try considerations)
      let
        failures =
            fst (partitionEithers results)

      return failures



runWithInvariants
    :: Monad m
    => [Pattern fs m ()]
    -> Pattern fs m result
    -> Pattern fs m result

runWithInvariants invariants method =
    let
      invariant =
          sequence (map try invariants)

      test (Pure r) =
          Pure r

      test (Fail e) =
          Fail e

      test (M m) =
          M (fmap test m)

      test (Step sym bp) =
          Step sym $ \value ->
              do
                results <- invariant
                let
                  failures =
                      fst (partitionEithers results)

                case failures of

                    [] ->
                        test (bp value)

                    xs ->
                        throw (Breaches During xs)

    in
      test method



contract
    :: forall variables result fs m.
       Monad m
    => Contract variables result fs m
    -> Pattern fs m result

contract (Contract considerations method) =
#ifndef NO_CONTRACTS
    do
      let
        (preconditions,invariants,postconditionals) =
            splitConsiderations considerations

      preFailures <- consider preconditions
      case preFailures of

          [] ->
              do
                result <- try (runWithInvariants invariants method)
                case result of

                    Left failure ->
                        throw (failure :: Breaches)

                    Right value ->
                        do
                          let
                            postconditions =
                                map ($ value) postconditionals

                          postFailures <- consider postconditions
                          case postFailures of

                              [] ->
                                  return value

                              _ ->
                                  throw (Breaches After postFailures)

          _ ->
              throw (Breaches Before preFailures)
#else
    method
#endif



splitConsiderations considerations =
    let
      splitter =
          consider precondition invariant postcondition

    in
      foldr splitter ([],[],[]) considerations
  where

    consider pre _ _ (Precondition consideration) =
        pre consideration

    consider _ inv _ (Invariant consideration) =
        inv consideration

    consider _ _ post (Postcondition consideration) =
        post consideration

    precondition pre ~(pres,invs,posts) =
        (pre:pres,invs,posts)

    invariant inv ~(pres,invs,posts) =
        (pres,inv:invs,posts)

    postcondition post ~(pres,invs,posts) =
        (pres,invs,post:posts)
