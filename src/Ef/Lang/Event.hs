{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Lang.Event
    where


import Ef.Core
import Ef.Core.Narrative
import Ef.Lang.Knot
import Ef.Lang.IO

import Control.Monad
import Data.IORef
import Unsafe.Coerce
import System.IO.Unsafe


data Reactor lexicon environment event =
    Reactor
        {
          become
              :: (event -> Narrative lexicon environment ())
              -> Narrative lexicon environment ()

        , die
              :: Narrative lexicon environment ()

        }



data Signal lexicon environment event
    where

        Signal
            :: !(IORef event)
            -> !(IORef [event -> Narrative lexicon environment ()])
            -> Signal lexicon environment event 



construct
    :: event
    -> Signal lexicon environment event

construct event =
    let
        !current =
            unsafePerformIO $ newIORef event

        !behaviors =
            unsafePerformIO $ newIORef []

    in
        Signal current behaviors



merge_
    :: Monad environment
    => (Signal lexicon environment event -> event -> Narrative lexicon environment ())
    -> Signal lexicon environment event
    -> Signal lexicon environment event
    -> Signal lexicon environment event

merge_ triggerMethod (Signal current0 behaviors0) (Signal current1 behaviors1) =
    let
        !initial =
            unsafePerformIO $ readIORef current0

        !current =
            unsafePerformIO $ newIORef initial

        !behaviors =
            unsafePerformIO $ newIORef []

        !signal =
            Signal current behaviors

        addBehaviorLeft =
            unsafePerformIO $ modifyIORef behaviors0 $ \bs ->
                let
                    newBehavior event =
                        triggerMethod signal event

                in
                    newBehavior:bs

        addBehaviorRight =
            unsafePerformIO $ modifyIORef behaviors1 $ \bs ->
                let
                    newBehavior event =
                        triggerMethod signal event

                in
                    newBehavior:bs

    in
        addBehaviorLeft `seq` addBehaviorRight `seq` signal



mapSignal_
    :: Monad environment
    => (Signal lexicon environment b -> b -> Narrative lexicon environment ())
    -> (a -> b)
    -> Signal lexicon environment a
    -> Signal lexicon environment b

mapSignal_ triggerMethod f (Signal current0 behaviors0) =
    let
        !initial =
            f $ unsafePerformIO $ readIORef current0

        !current = 
            unsafePerformIO $ newIORef initial

        !behaviors =
            unsafePerformIO $ newIORef []

        !signal =
            Signal current behaviors

        addBehavior =
            unsafePerformIO $ modifyIORef behaviors0 $ \bs ->
                let
                    newBehavior event =
                        triggerMethod signal (f event)

                in
                    newBehavior:bs

    in
        addBehavior `seq` signal



filterSignal_
    :: Monad environment
    => (Signal lexicon environment a -> a -> Narrative lexicon environment ())
    -> (a -> Bool)
    -> a
    -> Signal lexicon environment a
    -> Signal lexicon environment a

filterSignal_ triggerMethod predicate initial (Signal _ behaviors0) =
    let
        !current =
            unsafePerformIO $ newIORef initial

        !behaviors =
            unsafePerformIO $ newIORef []

        !signal =
            Signal current behaviors

        addBehavior =
            unsafePerformIO $ modifyIORef behaviors0 $ \bs ->
                let
                    newBehavior event =
                        if predicate event then
                            triggerMethod signal event
                        else
                            return ()

                in
                    newBehavior:bs

    in
        addBehavior `seq` signal



behavior_
    :: Monad environment
    => Signal lexicon environment event
    -> (event -> Narrative lexicon environment ())
    -> Narrative lexicon environment ()

behavior_ (Signal _ behaviors) newBehavior =
    let
        updateBehaviors =
            unsafePerformIO $ modifyIORef behaviors (newBehavior:)

    in
        updateBehaviors `seq` return ()



trigger_
    :: Monad environment
    => Signal lexicon environment event
    -> event
    -> Narrative lexicon environment ()

trigger_ (Signal current behaviors) event =
    let
        !writeCurrent =
            unsafePerformIO $ writeIORef current event

        !behavior =
            sequence_ $ map ($ event) $
                unsafePerformIO $ readIORef behaviors

    in
        writeCurrent `seq` behavior



data Event lexicon environment =
    Event
        {
          trigger
              :: forall event.
                 Signal lexicon environment event
              -> event
              -> Narrative lexicon environment ()

        , behavior
              :: forall event.
                 Signal lexicon environment event
              -> (Reactor lexicon environment event -> event -> Narrative lexicon environment ())
              -> Narrative lexicon environment ()

        , merge
              :: forall event.
                 Signal lexicon environment event
              -> Signal lexicon environment event
              -> Signal lexicon environment event

        , mapSignal
              :: forall a b.
                 (a -> b)
              -> Signal lexicon environment a
              -> Signal lexicon environment b

        , filterSignal
              :: forall a.
                 (a -> Bool)
              -> a
              -> Signal lexicon environment a
              -> Signal lexicon environment a
        }



data Action lexicon environment
    where

        Trigger
            :: Signal lexicon environment event
            -> event
            -> Action lexicon environment

        Become
            :: (event -> Narrative lexicon environment ())
            -> Action lexicon environment

        Die
            :: Action lexicon environment



event
    :: forall lexicon environment a.
       ( Monad environment
       , Knows Knots lexicon environment
       )
    => (Event lexicon environment -> Narrative lexicon environment a)
    -> Narrative lexicon environment a

event loop =
    let
        ev up =
            Event
                {
                  trigger =
                      \signal event ->
                          up (Trigger signal event)

                , behavior =
                      \signal b ->
                          let
                              reactor =
                                  Reactor
                                      {
                                        become =
                                            \newBehavior ->
                                                join $ up (Become newBehavior)

                                      , die =
                                            join $ up Die
                                            
                                      }

                          in
                              behavior_ signal (b reactor)
                              

                , merge =
                      \signall signalr ->
                          merge_ (\signal event -> join $ up (Trigger signal event)) signall signalr

                , mapSignal =
                      \f signal ->
                          mapSignal_ (\signal event -> join $ up (Trigger signal event)) f signal

                , filterSignal =
                      \predicate initial ->
                          filterSignal_ (\signal event -> join $ up (Trigger signal event)) predicate initial

                }

    in
        linearize $
            server +>> (knotted $ \up _ -> loop (ev up))
        where

            server initialRequest =
                knotted $ \_ dn ->
                    withRespond dn initialRequest
                where

                    withRespond respond =
                        go
                        where

                            go req =
                                case req of

                                    Trigger signal event ->
                                        undefined
