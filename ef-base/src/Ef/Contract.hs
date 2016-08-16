{-# LANGUAGE CPP #-}
module Ef.Contract
    ( contract
    , Consideration(..)
    , Breaches(..)
    , Contract(..)
    , Phase(..)
    ) where

import Ef

import Control.Exception (Exception(..), SomeException)
import Data.Either

data Phase = Before | During | After
  deriving Show

type MethodName = String

data Breaches = Breaches MethodName Phase [SomeException]
  deriving Show
instance Exception Breaches

data Consideration self super variables result
    = Precondition (Narrative self super ())
    | Postcondition (result -> Narrative self super ())
    | Invariant (Narrative self super ())

data Contract self super variables result
    = Contract [Consideration self super variables result]
               (Narrative self super result)

consider :: Monad super
         => [Narrative self super ()] -> Narrative self super [SomeException]
consider considerations = do
    results <- sequence (map try considerations)
    let failures = fst (partitionEithers results)
    return failures

runWithInvariants :: Monad super
                  => MethodName
                  -> [Narrative self super ()]
                  -> Narrative self super result
                  -> Narrative self super result
runWithInvariants methodName invariants method =
    let invariant = sequence (map try invariants)
        test (Return r) = Return r
        test (Fail e) = Fail e
        test (Super m) = Super (fmap test m)
        test (Say symbol k) =
            Say symbol $ \result ->
                do
                    results <- invariant
                    let failures = fst (partitionEithers results)
                    if null failures then
                        test (k result)
                    else
                        throwM (Breaches methodName During failures)
    in test method

contract
    :: forall variables result self super.
       Monad super
    => MethodName
    -> Contract self super variables result
    -> Narrative self super result
contract methodName (Contract considerations method) =
#ifdef CONTRACTS
    do
      let (preconditions,invariants,postconditionals) = splitConsiderations considerations
      preFailures <- consider preconditions
      case preFailures of

          [] -> do
              result <- try (runWithInvariants methodName invariants method)
              case result of

                  Left failure -> throw (failure :: Breaches)

                  Right value -> do
                      let postconditions = map ($ value) postconditionals
                      postFailures <- consider postconditions
                      case postFailures of

                          [] -> return value

                          _ -> throw (Breaches methodName After postFailures)

          _ -> throw (Breaches methodName Before preFailures)
#else
    method
#endif

splitConsiderations considerations =
    let splitter = consider precondition invariant postcondition
    in foldr splitter ([],[],[]) considerations
  where

    consider pre _ _ (Precondition consideration) = pre consideration
    consider _ inv _ (Invariant consideration) = inv consideration
    consider _ _ post (Postcondition consideration) = post consideration

    precondition pre ~(pres,invs,posts) = (pre:pres,invs,posts)

    invariant inv ~(pres,invs,posts) = (pres,inv:invs,posts)

    postcondition post ~(pres,invs,posts) = (pres,invs,post:posts)
