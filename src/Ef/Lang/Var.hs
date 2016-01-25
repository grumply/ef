{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
module Ef.Lang.Var
    ( Var(..)
    , var
    , var'

    , Eagerness(..)
    , Action(..)
    , stateful
    ) where



import Ef.Core.Narrative
import Ef.Lang.Knot

import Control.Monad



data Eagerness
    where

        Strict
            :: Eagerness

        Lazy
            :: Eagerness

    deriving Eq



data Action state
    where

        Modify
            :: Eagerness
            -> (state -> state)
            -> Action state

data Var var lexicon environment =
    Var
        {
          modify
              :: (var -> var)
              -> Narrative lexicon environment ()

        , modify'
              :: (var -> var)
              -> Narrative lexicon environment ()

        , get
              :: Narrative lexicon environment var

        , gets
              :: forall a.
                 (var -> a)
              -> Narrative lexicon environment a

        , put
              :: var
              -> Narrative lexicon environment ()

        , puts
              :: forall a.
                 (a -> var)
              -> a
              -> Narrative lexicon environment ()

        }



stateful
    :: Knows Knots lexicon environment
    => (Var state lexicon environment -> Narrative lexicon environment result)
    -> Knotted (Action state) state () X lexicon environment result

stateful computation =
    let
        stateInterface up =
            Var
                {
                  modify =
                      \mod ->
                          do
                              _ <- up (Modify Lazy mod)
                              return ()

                , modify' =
                      \mod ->
                          do
                              _ <- up (Modify Strict mod)
                              return ()

                , get =
                      up (Modify Lazy id)

                , gets =
                      \view ->
                          do
                              current <- up (Modify Lazy id)
                              return (view current)

                , put =
                      \new ->
                          let
                              update = const new

                          in
                              do
                                  up (Modify Lazy update)
                                  return ()

                , puts =
                      \view big ->
                          let
                              new = const (view big)

                          in
                              do
                                  _ <- up (Modify Lazy new)
                                  return ()

                }

    in
        knotted $ \up _ -> computation (stateInterface up)


var
    :: Knows Knots lexicon environment
    => state
    -> (Var state lexicon environment -> Narrative lexicon environment result)
    -> Narrative lexicon environment result

var initial computation =
    linearize (serve +>> stateful computation)
    where

        serve firstRequest =
            knotted $ \_ dn ->
                withRespond dn initial firstRequest
            where

                withRespond respond =
                    handle
                    where

                        handle current (Modify strictness mod) =
                            do
                                let
                                    new = mod current

                                    force = new `seq` return ()

                                when (strictness == Strict) force
                                next <- respond new
                                handle new next



var'
    :: Knows Knots lexicon environment
    => state
    -> (Var state lexicon environment -> Narrative lexicon environment result)
    -> Narrative lexicon environment result

var' initial computation =
    linearize (serve +>> stateful computation)
    where

        serve firstRequest =
            knotted $ \_ dn ->
                withRespond dn initial firstRequest
            where

                withRespond respond =
                    handle
                    where

                        handle !current (Modify _ mod) =
                            do
                                let
                                    new = mod current

                                next <- respond new
                                handle new next



{-# INLINE var #-}
{-# INLINE var' #-}
