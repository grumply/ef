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



type MethodName = String

data Breaches
  where

    Breaches
        :: MethodName
        -> Phase
        -> [SomeException]
        -> Breaches

  deriving Show
instance Exception Breaches



data Consideration lexicon environment variables result
  where

    Precondition
        :: Narrative lexicon environment ()
        -> Consideration lexicon environment variables result

    Postcondition
        :: (    result
             -> Narrative lexicon environment ()
           )
        -> Consideration lexicon environment variables result

    Invariant
        :: Narrative lexicon environment ()
        -> Consideration lexicon environment variables result




data Contract lexicon environment variables result
  where

    Contract
        :: [Consideration lexicon environment variables result]
        -> Narrative lexicon environment result
        -> Contract lexicon environment variables result



consider
    :: Monad environment
    => [Narrative lexicon environment ()]
    -> Narrative lexicon environment [SomeException]

consider considerations =
    do
      results <- sequence (map try considerations)
      let
        failures =
            fst (partitionEithers results)

      return failures



runWithInvariants
    :: Monad environment
    => MethodName
    -> [Narrative lexicon environment ()]
    -> Narrative lexicon environment result
    -> Narrative lexicon environment result

runWithInvariants methodName invariants method =
    let
      invariant =
          sequence (map try invariants)

      test (Return r) =
          Return r

      test (Fail e) =
          Fail e

      test (Super m) =
          Super (fmap test m)

      test (Say symbol k) =
          Say symbol $ \result ->
              do
                results <- invariant
                let
                  failures =
                      fst (partitionEithers results)

                if null failures then
                    test (k result)
                else
                    throw (Breaches methodName During failures)

    in
      test method



contract
    :: forall variables result lexicon environment.
       Monad environment
    => MethodName
    -> Contract lexicon environment variables result
    -> Narrative lexicon environment result

contract methodName (Contract considerations method) =
#ifdef CONTRACTS
    do
      let
        (preconditions,invariants,postconditionals) =
            splitConsiderations considerations

      preFailures <- consider preconditions
      case preFailures of

          [] ->
              do
                result <- try (runWithInvariants methodName invariants method)
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
                                  throw (Breaches methodName After postFailures)

          _ ->
              throw (Breaches methodName Before preFailures)
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
