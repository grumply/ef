{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Var
    ( Var(..)
    , var
    , var'

    , Eagerness(..)
    , Action(..)
    , stateful
    ) where


import Ef.Narrative
import Ef.Knot

import Control.Monad


data Eagerness
    = Strict
    | Lazy
    deriving Eq


data Action state = Modify Eagerness (state -> state)


data Var var self super =
    Var
        {
          modify
              :: (var -> var)
              -> Narrative self super ()

        , modify'
              :: (var -> var)
              -> Narrative self super ()

        , get
              :: Narrative self super var

        , gets
              :: forall a.
                 (var -> a)
              -> Narrative self super a

        , put
              :: var
              -> Narrative self super ()

        , puts
              :: forall a.
                 (a -> var)
              -> a
              -> Narrative self super ()

        }



stateful
    :: ('[Knot] <: self, Monad super)
    => (Var state self super -> Narrative self super result)
    -> Knotted (Action state) state () X self super result

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
    :: ('[Knot] <: self, Monad super)
    => state
    -> (Var state self super -> Narrative self super result)
    -> Narrative self super result

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
    :: ('[Knot] <: self, Monad super)
    => state
    -> (Var state self super -> Narrative self super result)
    -> Narrative self super result

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
