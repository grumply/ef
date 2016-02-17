{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Event (trigger_, Reactor(..), Signal, construct, Event(..), event) where


import Ef
import Ef.Narrative
import Ef.Single
import Ef.IO

import Control.Monad
import Data.IORef
import Unsafe.Coerce


data Reactor self super event =
    Reactor
        {
          become
              :: (event -> Narrative self super ())
              -> Narrative self super ()

        , die
              :: Narrative self super ()

        }


data Signal self super event
    = Signal !(IORef event) !(IORef [event -> Narrative self super ()])


-- | Construct a new signal with an initial value. Undefined is a valid
-- start value as long as any behaviors linked to the `Signal` are not
-- linked via `behavior'`.
construct :: (Lift IO super,Monad super) => event -> super (Signal self super' event)
construct event = do
    current <- lift $ newIORef event
    behaviors <- lift $ newIORef []
    return $ Signal current behaviors


merge_
    :: forall self super event.
       (Lift IO super, Monad super)
    => (Signal self super event -> event -> Narrative self super ())
    -> event
    -> Signal self super event
    -> Signal self super event
    -> Narrative self super (Signal self super event)

merge_ triggerMethod initial (Signal current0 behaviors0) (Signal current1 behaviors1) = do
    signal <- construct initial
    lift $ modifyIORef behaviors0 $ \bs ->
        let newBehavior event = triggerMethod signal event
        in newBehavior:bs
    lift $ modifyIORef behaviors1 $ \bs ->
        let newBehavior event = triggerMethod signal event
        in newBehavior:bs
    return signal



mapSignal_
    :: (Monad super, Lift IO super)
    => (Signal self super b -> b -> Narrative self super ())
    -> (a -> b)
    -> b
    -> Signal self super a
    -> Narrative self super (Signal self super b)

mapSignal_ triggerMethod f initial (Signal current0 behaviors0) = do
    signal <- construct initial
    lift $ modifyIORef behaviors0 $ \bs ->
        let newBehavior event = triggerMethod signal (f event)
        in newBehavior:bs
    return signal



filterSignal_
    :: (Monad super, Lift IO super)
    => (Signal self super a -> a -> Narrative self super ())
    -> (a -> Bool)
    -> a
    -> Signal self super a
    -> Narrative self super (Signal self super a)

filterSignal_ triggerMethod predicate initial (Signal _ behaviors0) = do
    signal <- construct initial
    lift $ modifyIORef behaviors0 $ \bs ->
        let newBehavior event =
                if predicate event then
                    triggerMethod signal event
                else
                    return ()
        in newBehavior:bs
    return signal



behavior_
    :: (Monad super, Lift IO super)
    => Signal self super event
    -> (event -> Narrative self super ())
    -> Narrative self super ()

behavior_ (Signal _ behaviors) newBehavior =
    lift $ modifyIORef behaviors (newBehavior:)



trigger_
    :: (Monad super, Lift IO super)
    => Signal self super event
    -> event
    -> Narrative self super ()

trigger_ (Signal current behaviors) event = do
    writeCurrent <- lift $ writeIORef current event
    bs <- lift $ readIORef behaviors
    let applied = map ($ event) bs
    sequence_ applied



data Event self super =
    Event
        { -- | Trigger an event on a signal; behaviors connected to the signal
          -- will be run in place.
          trigger
              :: forall event.
                 Signal self super event
              -> event
              -> Narrative self super ()

          -- | Create a new reactive behavior connected to that signal.
          -- The behaviors are given a `Reactor` interface to permit statefulness
          -- and self-death.
        , behavior
              :: forall event.
                 Signal self super event
              -> (Reactor self super event -> event -> Narrative self super ())
              -> Narrative self super ()

          -- | Same as `behavior`, but reacts immediately to the current value
          -- of the signal, as well.
        , behavior'
              :: forall event.
                 Signal self super event
              -> (Reactor self super event -> event -> Narrative self super ())
              -> Narrative self super ()

          -- | merge two `Signal`s; any time either `Signal` is `trigger`ed, the
          -- new signal will be triggered with that event.
        , mergeSignals
              :: forall event.
                 event
              -> Signal self super event
              -> Signal self super event
              -> Narrative self super (Signal self super event)

          -- | create a new `Signal` by mapping a function over an existing `Signal`.
        , mapSignal
              :: forall a b.
                 (a -> b)
              -> b
              -> Signal self super a
              -> Narrative self super (Signal self super b)

          -- | create a new `Signal` by filtering an existing `Signal` with a predicative function.
        , filterSignal
              :: forall a.
                 (a -> Bool)
              -> a
              -> Signal self super a
              -> Narrative self super (Signal self super a)
        }



data Action self super
    where

        Trigger
            :: (    Action self super
                 -> Narrative self super (Narrative self super ())
               )
            -> Signal self super event
            -> event
            -> Action self super

        Continue
            :: Action self super

        Become
            :: (event -> Narrative self super ())
            -> Action self super

        Die
            :: Action self super


-- | Scope an event framework to work with `Signal`s.
--
-- @
--     let sig1 = construct (0 :: Int)
--         sig2 = construct 'a'
--     event $ \Event{..} -> do
--         behavior sig1 $ \\n -> trigger sig2 (toEnum n)
--         behavior sig2 $ \\ch -> do
--             if ch == 'z' then io (putChar '\n') >> die
--                        else io (putChar ch)
--         mapM_ (trigger sig1) [1..100]
-- @
event
    :: forall self super a.
       (Knows SingleKnot self super, Lift IO super)
    => (Event self super -> Narrative self super a)
    -> Narrative self super a

event loop =
    let
        ev
            :: (   Action self super
                -> Narrative self super (Narrative self super ())
               )
            -> Event self super

        ev up =
            Event { trigger =
                        \signal event ->
                            join $ up (Trigger up signal event)

                  , behavior =
                        \signal b ->
                            let reactor = Reactor
                                    { become =
                                          \newBehavior ->
                                              join $ up (Become newBehavior)
                                    , die = join $ up Die
                                    }
                            in behavior_ signal (b reactor)

                  , behavior' =
                         \signal@(Signal currentRef _) b ->
                             let reactor = Reactor
                                     { become =
                                           \newBehavior ->
                                               join $ up (Become newBehavior)
                                     , die = join $ up Die
                                     }
                             in do current <- lift $ readIORef currentRef
                                   behavior_ signal (b reactor)
                                   b reactor current

                  , mergeSignals =
                        merge_ (\signal event ->
                                         join $ up (Trigger up signal event)
                               )

                  , mapSignal =
                            mapSignal_ (\signal event ->
                                             join $ up (Trigger up signal event)
                                       )

                  , filterSignal =
                        filterSignal_ (\signal event ->
                                                join $
                                                    up (Trigger up signal event)
                                          )
                  }
    in linearize $ server +>> (knotted $ \up _ -> loop (ev up))
    where

        server initialRequest =
            knotted $ \_ dn -> withRespond dn initialRequest
            where

                withRespond
                    :: (    Narrative self super ()
                         -> Narrative self super (Action self super)
                       )
                    -> Action self super
                    -> Narrative self super a

                withRespond respond =
                    eventLoop
                    where

                        eventLoop :: Action self super -> Narrative self super a
                        eventLoop req =
                            case req of

                                Trigger up (Signal currentRef behaviorsRef) event -> do
                                    behaviors <- lift $ readIORef behaviorsRef
                                    current <- lift $ writeIORef currentRef event
                                    newBehaviors <- runBehaviors up event behaviors
                                    lift $ writeIORef behaviorsRef newBehaviors
                                    newRequest <- respond (return ())
                                    eventLoop newRequest

                        runBehaviors
                            :: forall event.
                               (    Action self super
                                 -> Narrative self super (Narrative self super ())
                               )
                            -> event
                            -> [event -> Narrative self super ()]
                            -> Narrative self super [event -> Narrative self super ()]

                        runBehaviors up event =
                            withAcc []
                            where

                                withAcc acc [] = return (reverse acc)

                                withAcc acc (behavior0:behaviors) = do
                                    newRequest <- respond $ do behavior0 event
                                                               join (up Continue)
                                    withBehavior behavior0 True newRequest
                                    where

                                        withBehavior behavior alive request =
                                            case request of

                                                Trigger up' (Signal currentRef behaviorsRef) event' -> do
                                                    behaviors' <- lift $ readIORef behaviorsRef
                                                    current <- lift $ writeIORef currentRef event'
                                                    newBehaviors <- runBehaviors up' event' behaviors'
                                                    lift $ writeIORef behaviorsRef newBehaviors
                                                    newRequest <- respond (return ())
                                                    withBehavior behavior alive newRequest

                                                Become f -> do
                                                    newRequest <- respond (return ())
                                                    withBehavior (unsafeCoerce f) alive newRequest

                                                Die -> do
                                                    newRequest <- respond (return ())
                                                    withBehavior behavior False newRequest

                                                Continue ->
                                                    if alive then
                                                        withAcc (behavior:acc) behaviors
                                                    else
                                                        withAcc acc behaviors


{-# INLINE event #-}
