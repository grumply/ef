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



data Consideration variables result scope parent
  where

    Precondition
        :: Pattern scope parent ()
        -> Consideration variables result scope parent

    Postcondition
        :: (    result
             -> Pattern scope parent ()
           )
        -> Consideration variables result scope parent

    Invariant
        :: Pattern scope parent ()
        -> Consideration variables result scope parent




data Contract variables result scope parent
  where

    Contract
        :: [Consideration variables result scope parent]
        -> Pattern scope parent result
        -> Contract variables result scope parent



consider
    :: Monad parent
    => [Pattern scope parent ()]
    -> Pattern scope parent [SomeException]

consider considerations =
    do
      results <- sequence (map try considerations)
      let
        failures =
            fst (partitionEithers results)

      return failures



runWithInvariants
    :: Monad parent
    => [Pattern scope parent ()]
    -> Pattern scope parent result
    -> Pattern scope parent result

runWithInvariants invariants method =
    let
      invariant =
          sequence (map try invariants)

      test (Pure r) =
          Pure r

      test (Fail e) =
          Fail e

      test (Super m) =
          Super (fmap test m)

      test (Send symbol k) =
          Send symbol $ \result ->
              do
                results <- invariant
                let
                  failures =
                      fst (partitionEithers results)

                if null failures then
                    test (k result)
                else
                    throw (Breaches During failures)

    in
      test method



contract
    :: forall variables result scope parent.
       Monad parent
    => Contract variables result scope parent
    -> Pattern scope parent result

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
