{-# LANGUAGE RecordWildCards #-}
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
import Ef.Lang.Exit
import Control.Monad
import Data.IORef
import Unsafe.Coerce
import System.IO.Unsafe


import Ef.Lang.Knot.Context

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

        , behavior' 
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
            :: (Action lexicon environment -> Narrative lexicon environment (Narrative lexicon environment ()))
            -> Signal lexicon environment event
            -> event
            -> Action lexicon environment

        Continue
            :: Action lexicon environment

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
        ev
            :: (Action lexicon environment -> Narrative lexicon environment (Narrative lexicon environment ()))
            -> Event lexicon environment

        ev up =
            Event
                {
                  trigger =
                      \signal event ->
                          join $ up (Trigger up signal event)

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

                , behavior' =
                      \signal@(Signal currentRef _) b ->
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
                              do
                                  let
                                      current =
                                          unsafePerformIO $ readIORef currentRef
                                  behavior_ signal (b reactor)
                                  b reactor current
                                  
                , merge =
                      \signall signalr ->
                          merge_ (\signal event -> join $ up (Trigger up signal event)) signall signalr

                , mapSignal =
                      \f signal ->
                          mapSignal_ (\signal event -> join $ up (Trigger up signal event)) f signal

                , filterSignal =
                      \predicate initial ->
                          filterSignal_ (\signal event -> join $ up (Trigger up signal event)) predicate initial

                }

    in
        linearize $
            server +>> (knotted $ \up _ -> loop (ev up))
        where

            server initialRequest =
                knotted $ \_ dn ->
                    withRespond dn initialRequest
                where

                    withRespond
                        :: (Narrative lexicon environment () -> Narrative lexicon environment (Action lexicon environment))
                        -> Action lexicon environment
                        -> Narrative lexicon environment a

                    withRespond respond =
                        eventLoop
                        where

                            eventLoop
                                :: Action lexicon environment
                                -> Narrative lexicon environment a

                            eventLoop req =
                                case req of

                                    Trigger up (Signal currentRef behaviorsRef) event ->
                                        do
                                            let
                                                !behaviors =
                                                    unsafePerformIO $ readIORef behaviorsRef

                                                !current =
                                                    unsafePerformIO $ writeIORef currentRef event

                                            newBehaviors <- behaviors `seq` current `seq` runBehaviors up event behaviors
                                            let
                                                writeNewBehaviors =
                                                    unsafePerformIO $ writeIORef behaviorsRef newBehaviors

                                            newRequest <- writeNewBehaviors `seq` respond (return ())
                                            eventLoop newRequest

                            runBehaviors
                                :: forall event.
                                   (Action lexicon environment -> Narrative lexicon environment (Narrative lexicon environment ()))
                                -> event
                                -> [event -> Narrative lexicon environment ()]
                                -> Narrative lexicon environment [event -> Narrative lexicon environment ()]

                            runBehaviors up event =
                                withAcc []
                                where

                                    withAcc acc [] =
                                         return (reverse acc)

                                    withAcc acc (behavior0:behaviors) =
                                         do
                                             newRequest <- respond (behavior0 event >> join (up Continue))
                                             withBehavior behavior0 True newRequest
                                         where

                                             withBehavior behavior alive request =
                                                 case request of

                                                     Trigger up' (Signal currentRef behaviorsRef) event' ->
                                                         do
                                                             let
                                                                 !behaviors' =
                                                                     unsafePerformIO $ readIORef behaviorsRef

                                                                 !current =
                                                                     unsafePerformIO $ writeIORef currentRef event'

                                                             newBehaviors <- behaviors' `seq` current `seq` runBehaviors up' event' behaviors'
                                                             let
                                                                 writeNewBehaviors =
                                                                     unsafePerformIO $ writeIORef behaviorsRef newBehaviors

                                                             newRequest <- writeNewBehaviors `seq` respond (return ())
                                                             withBehavior behavior alive newRequest

                                                     Become f ->
                                                         do
                                                             newRequest <- respond (return ())
                                                             withBehavior (unsafeCoerce f) alive newRequest

                                                     Die ->
                                                         do
                                                             newRequest <- respond (return ())
                                                             withBehavior behavior False newRequest

                                                     Continue ->
                                                         if alive then
                                                             withAcc (behavior:acc) behaviors
                                                         else
                                                             withAcc acc behaviors



main =
    do
        let
            obj = Object $ knots *:* Empty

        (_,result) <- delta obj $
            do
                event $ \Event{..} ->
                    do
                        let
                            sig0 = construct (0 :: Int)
                            sig1 = mapSignal (+1) sig0

                        behavior' sig1 (\Reactor{..} e -> if e == 2 then become (const die) else io (print e))
                        trigger sig0 0
                        trigger sig0 1
                        trigger sig0 5
                        trigger sig0 5
                        return "Done"
        print result



{-# INLINE event #-}
