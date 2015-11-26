{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.Lang.Contract
    ( contract
    ) where



import Ef.Core

import Ef.Lang.Scoped.Diverge

import Control.Exception
    ( Exception(..)
    , SomeException
    )

import Control.Monad
    ( unless
    )

import Data.Either

import Data.Typeable


{-
One problem with this approach is the ability of conditional analyses
to modify the object pre- and post- execution via Diverging.
It is the responsibility of the contract designer to avoid this.
-}


data Breaches
  where

    Breaches
        :: [SomeException]
        -> Breaches

  deriving Show
instance Exception Breaches



data Consideration variables result fs m
  where

    -- Preconditions represent analyses pre-execution.
    Precondition
        :: (    variables
             -> Pattern fs m ()
           )
        -> Consideration variables result fs m

    -- Postconditions represent analyses post-execution.
    Postcondition
        :: (    result
             -> Pattern fs m ()
           )
        -> Consideration variables result fs m

    -- Invariants represent analyses during execution.
    Invariant
        :: Pattern fs m ()
        -> Consideration variables result fs m

    -- Consideration represents analyses after execution
    -- that include the variables.
    Consideration
        :: (    variables
             -> result
             -> Pattern fs m ()
           )
        -> Consideration variables result fs m



data Contract variables result fs m
  where

    Contract
        :: [Consideration variables result fs m]
        -> (    variables
             -> Pattern fs m result
           )
        -> Contract variables result fs m



runPreconditions
    :: Monad m
    => Contract variables result fs m
    -> variables
    -> Pattern fs m ()

runPreconditions (Contract conditionals _) variables =
    do
      let
        isPrecondition (Precondition _) =
            True

        isPrecondition _ =
            False

        preconditions =
            filter isPrecondition conditionals

        applyTest (Precondition test) =
            test variables

        tryPreconditionals =
            map (try . applyTest) preconditions

      results <- sequence tryPreconditionals
      let
        accumulate (Left err) rest =
            err:rest

        accumulate _ rest =
            rest

        failures =
            foldr accumulate [] results

      case failures of

          [] ->
              return ()

          xs ->
              throw (Breaches xs)


contract
    :: forall variables result fs m.
       Contract variables result fs m
    -> variables
    -> Pattern fs m result

contract contract variables =
#ifndef NO_CONTRACTS
    do
      _ <- runPreconditions contract variables
      return _


#else
--  do
#endif
--     a <- method vs
-- #ifndef NO_CONTRACTS
--     postconditionResult <- postcondition a
--     unless postconditionResult $ do
--       ty <- typeOfSelf
--       throw (Contract ("contract: post-condition failed: " ++ post ++ "\n\tin: " ++ show ty))
-- #endif
--     return a
-- #ifndef NO_CONTRACTS
--   where
--     c (Contract str) = error str
-- #endif
