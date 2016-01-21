{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Ef.Lang.Var
    ( Var
    , var
    ) where



import Ef.Core.Narrative
import Ef.Lang.IO
import Ef.Lang.Knot

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



var
    :: Knows Knots lexicon environment
    => state
    -> (    Var state lexicon environment
         -> Narrative lexicon environment a
       )
    -> Narrative lexicon environment a

var initial stateful =
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
