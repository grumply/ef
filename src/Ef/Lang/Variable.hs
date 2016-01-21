{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main
    ( Variable
    , state
    ) where



import Ef.Core.Narrative
import Ef.Lang.IO
import Ef.Lang.Knot

import Ef.Core
import Ef.Core.Object
import Ef.Lang.Knot.Context

import Control.Monad
import Unsafe.Coerce



data Eagerness
    where

        Strict
            :: Eagerness

        Lazy
            :: Eagerness

    deriving Eq



data Modify
    where

        Modify
            :: Eagerness
            -> (state -> state)
            -> Modify

data Variable var lexicon environment =
    Variable
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



state
    :: Knows Knots lexicon environment
    => state
    -> (    Variable state lexicon environment
         -> Narrative lexicon environment a
       )
    -> Narrative lexicon environment a

state initial stateful =
        linearize (serve +>> consume)

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
                                    new = (unsafeCoerce mod) current

                                    force = new `seq` return ()

                                when (strictness == Strict) force
                                next <- respond new
                                handle new next
                            

        consume =
            let

                stateInterface up =
                    Variable
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
                                      update =
                                          const new

                                  in
                                      do
                                          up (Modify Lazy update)
                                          return ()

                        , puts =
                              \view big ->
                                  let
                                      new =
                                          const (view big)

                                  in
                                      do
                                          _ <- up (Modify Lazy new)
                                          return ()

                        }

            in
                knotted $ \up _ -> stateful (stateInterface up)
