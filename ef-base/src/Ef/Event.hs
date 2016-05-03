module Ef.Event (signal_, Reactor(..), Signal(..), construct, Event(..), event, BehaviorToken(..), clearSignal) where


import Ef
import Ef.Narrative
import Ef.Bidir
import Ef.IO

import Control.Monad
import Data.IORef
import Data.List
import Unsafe.Coerce


data Reactor self super event =
    Reactor
        {
          become
              :: (event -> Narrative self super ())
              -> Narrative self super ()

        , end
              :: Narrative self super ()

        }


data Signal self super event
    = Signal (IORef event) (IORef Int) (IORef [(Int,event -> Narrative self super ())])
    deriving Eq


-- | Construct a new signal with an initial value. Undefined is a valid
-- start value as long as any behaviors linked to the `Signal` are not
-- linked via `behavior'`.
construct :: (Lift IO super,Monad super) => event -> super (Signal self super' event)
construct event = do
    current <- lift $ newIORef event
    count <- lift $ newIORef 0
    behaviors <- lift $ newIORef []
    return $ Signal current count behaviors


clearSignal :: (Lift IO super, Monad super) => Signal self super' e -> super ()
clearSignal (Signal _ _ bs) = lift $ writeIORef bs []

merge_
    :: forall self super event.
       (Lift IO super, Monad super)
    => (Signal self super event -> event -> Narrative self super ())
    -> event
    -> Signal self super event
    -> Signal self super event
    -> Narrative self super (Signal self super event,BehaviorToken self super event, BehaviorToken self super event)

merge_ signalMethod initial sig0@(Signal current0 count0 behaviors0) sig1@(Signal current1 count1 behaviors1) = do
    signal <- construct initial
    c0 <- lift $ atomicModifyIORef count0 $ \c -> (c + 1,c)
    lift $ modifyIORef behaviors0 $ \bs ->
        let newBehavior event = signalMethod signal event
        in bs ++ [(c0,newBehavior)]
    c1 <- lift $ atomicModifyIORef count1 $ \c -> (c + 1,c)
    lift $ modifyIORef behaviors1 $ \bs ->
        let newBehavior event = signalMethod signal event
        in bs ++ [(c1,newBehavior)]
    return (signal,BehaviorToken c0 sig0,BehaviorToken c1 sig1)



mapSignal_
    :: (Monad super, Lift IO super)
    => (Signal self super b -> b -> Narrative self super ())
    -> (a -> b)
    -> b
    -> Signal self super a
    -> Narrative self super (Signal self super b, BehaviorToken self super b)

mapSignal_ signalMethod f initial sig@(Signal current0 count0 behaviors0) = do
    signal <- construct initial
    c <- lift $ atomicModifyIORef count0 $ \c -> (c + 1,c)
    lift $ modifyIORef behaviors0 $ \bs ->
        let newBehavior event = signalMethod signal (f event)
        in bs ++ [(c,newBehavior)]
    return (signal,BehaviorToken c signal)



filterSignal_
    :: (Monad super, Lift IO super)
    => (Signal self super a -> a -> Narrative self super ())
    -> (a -> Bool)
    -> a
    -> Signal self super a
    -> Narrative self super (Signal self super a,BehaviorToken self super a)

filterSignal_ signalMethod predicate initial sig@(Signal _ count0 behaviors0) = do
    signal <- construct initial
    c <- lift $ atomicModifyIORef count0 $ \c -> (c + 1,c)
    lift $ modifyIORef behaviors0 $ \bs ->
        let newBehavior event =
                if predicate event then
                    signalMethod signal event
                else
                    return ()
        in bs ++ [(c,newBehavior)]
    return (signal,BehaviorToken c signal)



behavior_
    :: (Monad super, Lift IO super)
    => Signal self super event
    -> (event -> Narrative self super ())
    -> Narrative self super (BehaviorToken self super event)

behavior_ sig@(Signal _ count behaviors) newBehavior = do
    c <- lift $ atomicModifyIORef count $ \c -> (c + 1,c)
    lift $ modifyIORef behaviors (++ [(c,newBehavior)])
    return (BehaviorToken c sig)

stop_
    :: (Monad super, Lift IO super)
    => BehaviorToken self super event
    -> Narrative self super ()
stop_ (BehaviorToken bt (Signal _ _ behaviors)) =
    lift $ modifyIORef behaviors $ filter ((/=) bt . fst)

signal_
    :: (Monad super, Lift IO super)
    => Signal self super event
    -> event
    -> Narrative self super ()

signal_ (Signal current count behaviors) event = do
    writeCurrent <- lift $ writeIORef current event
    bs <- lift $ readIORef behaviors
    let applied = map (($ event) . snd) bs
    sequence_ applied

trigger_
    :: (Monad super, Lift IO super)
    => BehaviorToken self super event
    -> event
    -> Narrative self super ()
trigger_ (BehaviorToken bt (Signal current count behaviors)) event = do
    bs <- lift $ readIORef behaviors
    let (eq,neq) = partition ((==) bt . fst) bs
    case eq of
        [x] -> snd x event
        [] -> return ()


data BehaviorToken self super event = BehaviorToken Int (Signal self super event)
  deriving Eq

data Event self super =
    Event
        { trigger
              :: forall event.
                 BehaviorToken self super event
              -> event
              -> Narrative self super ()

          -- | Signal an event on a signal; behaviors connected to the signal
          -- will be run in place.
        , signal
              :: forall event.
                 Signal self super event
              -> event
              -> Narrative self super ()

        , enact
              :: forall event.
                 BehaviorToken self super event
              -> Narrative self super ()

          -- | Create a new reactive behavior connected to that signal.
          -- The behaviors are given a `Reactor` interface to permit statefulness
          -- and self-death.
        , behavior
              :: forall event.
                 Signal self super event
              -> (Reactor self super event -> event -> Narrative self super ())
              -> Narrative self super (BehaviorToken self super event)

        , stop
              :: forall event.
                 BehaviorToken self super event
              -> Narrative self super ()

          -- | merge two `Signal`s; any time either `Signal` is `signal`ed, the
          -- new signal will be signaled with that event.
        , mergeSignals
              :: forall event.
                 event
              -> Signal self super event
              -> Signal self super event
              -> Narrative self super (Signal self super event,BehaviorToken self super event,BehaviorToken self super event)

          -- | create a new `Signal` by mapping a function over an existing `Signal`.
        , mapSignal
              :: forall a b.
                 (a -> b)
              -> b
              -> Signal self super a
              -> Narrative self super (Signal self super b,BehaviorToken self super b)

          -- | create a new `Signal` by filtering an existing `Signal` with a predicative function.
        , filterSignal
              :: forall a.
                 (a -> Bool)
              -> a
              -> Signal self super a
              -> Narrative self super (Signal self super a,BehaviorToken self super a)
        }



data Action self super
    where

        Trigger
            :: (    Action self super
                 -> Narrative self super (Narrative self super ())
               )
            -> BehaviorToken self super event
            -> event
            -> Action self super

        Signal_
            :: (    Action self super
                 -> Narrative self super (Narrative self super ())
               )
            -> Signal self super event
            -> event
            -> Action self super

        Enact
            :: (    Action self super
                 -> Narrative self super (Narrative self super ())
               )
            -> BehaviorToken self super event
            -> Action self super

        Continue
            :: Action self super

        Become
            :: (event -> Narrative self super ())
            -> Action self super

        End
            :: Action self super

data Hole

-- | Scope an event framework to work with `Signal`s.
--
-- @
--     let sig1 = construct (0 :: Int)
--         sig2 = construct 'a'
--     event $ \Event{..} -> do
--         behavior sig1 $ \\n -> signal sig2 (toEnum n)
--         behavior sig2 $ \\ch -> do
--             if ch == 'z' then io (putChar '\n') >> end
--                        else io (putChar ch)
--         mapM_ (signal sig1) [1..100]
-- @
event
    :: forall self super a.
       ('[Bidir] :> self, Monad super, Lift IO super)
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
                        \bt event ->
                            join $ up (Trigger up bt event)

                  , signal =
                        \signal event ->
                            join $ up (Signal_ up signal event)

                  , enact =
                        \bt ->
                            join $ up (Enact up bt)

                  , behavior =
                        \signal b ->
                            let reactor = Reactor
                                    { become =
                                          \newBehavior ->
                                              join $ up (Become newBehavior)
                                    , end = join $ up End
                                    }
                            in behavior_ signal (b reactor)

                  , stop = stop_

                  , mergeSignals =
                        merge_ (\signal event -> join $ up (Signal_ up signal event))

                  , mapSignal =
                        mapSignal_
                            (\signal event -> join $ up (Signal_ up signal event))

                  , filterSignal =
                        filterSignal_
                            (\signal event -> join $ up (Signal_ up signal event))
                  }
    in linearize $ server +>> (knotted $ \up _ -> loop (ev up))
    where

        server :: forall x. Action self super -> Bi X () (Action self super) (Narrative self super ()) self super x
        server initialRequest =
            knotted $ \_ dn -> withRespond dn initialRequest
            where

                withRespond
                    :: (    Narrative self super ()
                         -> Narrative self super (Action self super)
                       )
                    -> Action self super
                    -> Narrative self super x

                withRespond respond =
                    eventLoop
                    where

                        eventLoop :: Action self super -> Narrative self super x
                        eventLoop req =
                            case req of

                                Signal_ up (Signal currentRef _ behaviorsRef) event -> do
                                    behaviors <- lift $ readIORef behaviorsRef
                                    lift $ writeIORef currentRef event
                                    newBehaviors <- runBehaviors up event behaviors
                                    lift $ writeIORef behaviorsRef newBehaviors
                                    newRequest <- respond (return ())
                                    eventLoop newRequest

                                Enact up (BehaviorToken bt (Signal currentRef _ behaviorsRef)) -> do
                                    behaviors <- lift $ readIORef behaviorsRef
                                    let (eq,neq) = partition ((==) bt . fst) behaviors -- will be ([] or [x],xs)
                                    current <- lift $ readIORef currentRef
                                    newBehaviors <- runBehaviors up current eq
                                    lift $ writeIORef behaviorsRef (newBehaviors ++ neq)
                                    newRequest <- respond (return ())
                                    eventLoop newRequest

                                Trigger up (BehaviorToken bt (Signal _ _ behaviorsRef)) event -> do
                                    behaviors <- lift $ readIORef behaviorsRef
                                    let (eq,neq) = partition ((==) bt . fst) behaviors
                                    newBehaviors <- runBehaviors up event eq
                                    lift $ writeIORef behaviorsRef (newBehaviors ++ neq)
                                    newRequest <- respond (return ())
                                    eventLoop newRequest

                        runBehaviors
                            :: forall event.
                               (    Action self super
                                 -> Narrative self super (Narrative self super ())
                               )
                            -> event
                            -> [(Int,event -> Narrative self super ())]
                            -> Narrative self super [(Int,event -> Narrative self super ())]

                        runBehaviors up event =
                            withAcc []
                            where

                                withAcc acc [] = return (reverse acc)

                                withAcc acc (behavior0:behaviors) = do
                                    newRequest <- respond $ do snd behavior0 event
                                                               join (up Continue)
                                    withBehavior behavior0 True newRequest
                                    where

                                        withBehavior (bt,behavior) alive request =
                                            case request of

                                                Signal_ up' (Signal currentRef _ behaviorsRef) event' -> do
                                                    behaviors' <- lift $ readIORef behaviorsRef
                                                    lift $ writeIORef currentRef event'
                                                    newBehaviors <- runBehaviors up' event' behaviors'
                                                    lift $ writeIORef behaviorsRef newBehaviors
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,behavior) alive newRequest

                                                Enact up' (BehaviorToken bt (Signal currentRef _ behaviorsRef)) -> do
                                                    behaviors <- lift $ readIORef behaviorsRef
                                                    let (eq,neq) = partition ((==) bt . fst) behaviors -- will be ([] or [x],xs)
                                                    current <- lift $ readIORef currentRef
                                                    newBehaviors <- runBehaviors up current eq
                                                    lift $ writeIORef behaviorsRef (newBehaviors ++ neq)
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,behavior) alive newRequest

                                                Trigger up (BehaviorToken bt' (Signal _ _ behaviorsRef)) event -> do
                                                    behaviors <- lift $ readIORef behaviorsRef
                                                    let (eq,neq) = partition ((==) bt' . fst) behaviors
                                                    newBehaviors <- runBehaviors up event eq
                                                    lift $ writeIORef behaviorsRef (newBehaviors ++ neq)
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,behavior) alive newRequest

                                                Become f -> do
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,unsafeCoerce f) alive newRequest

                                                End -> do
                                                    newRequest <- respond (return ())
                                                    withBehavior (bt,behavior) False newRequest

                                                Continue ->
                                                    if alive then
                                                        withAcc ((bt,behavior):acc) behaviors
                                                    else
                                                        withAcc acc behaviors


{-# INLINE event #-}
