module Ef.Var
    ( Var(..)
    , var
    , var'

    , Eagerness(..)
    , stateful
    ) where

import Ef.Narrative
import Ef.Sync

import Control.Monad

data Eagerness
    = Strict
    | Lazy
    deriving Eq

data Action state = Modify Eagerness (state -> state)

data Var var self super =
    Var
        {
          alter
              :: (var -> var)
              -> Narrative self super ()

        , alter'
              :: (var -> var)
              -> Narrative self super ()

        , peek
              :: Narrative self super var

        , peeks
              :: forall a.
                 (var -> a)
              -> Narrative self super a

        , poke
              :: var
              -> Narrative self super ()

        , pokes
              :: forall a.
                 (a -> var)
              -> a
              -> Narrative self super ()

        }

stateful :: ('[Sync Int] <: self, Monad super)
         => (Var state self super -> Narrative self super result)
         -> Synchronized Int (Action state) state () X self super result
stateful computation =
    let
        stateInterface up =
            Var
                {
                  alter =
                      \mod ->
                          do
                              _ <- up (Modify Lazy mod)
                              return ()

                , alter' =
                      \mod ->
                          do
                              _ <- up (Modify Strict mod)
                              return ()

                , peek =
                      up (Modify Lazy id)

                , peeks =
                      \view ->
                          do
                              current <- up (Modify Lazy id)
                              return (view current)

                , poke =
                      \new ->
                          let
                              update = const new

                          in
                              do
                                  up (Modify Lazy update)
                                  return ()

                , pokes =
                      \view big ->
                          let
                              new = const (view big)

                          in
                              do
                                  _ <- up (Modify Lazy new)
                                  return ()

                }

    in
        synchronized $ \up _ -> computation (stateInterface up)
{-# INLINE stateful #-}

var :: ('[Sync Int] <: self, Monad super)
    => state
    -> (Var state self super -> Narrative self super result)
    -> Narrative self super result
var initial computation =
    runSync (serve +>> stateful computation)
    where

        serve firstRequest =
            synchronized $ \_ dn ->
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
{-# INLINE var #-}

var' :: ('[Sync Int] <: self, Monad super)
     => state
     -> (Var state self super -> Narrative self super result)
     -> Narrative self super result
var' initial computation =
    runSync (serve +>> stateful computation)
    where

        serve firstRequest =
            synchronized $ \_ dn ->
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
{-# INLINE var' #-}
