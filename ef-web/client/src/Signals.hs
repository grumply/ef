{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Signals
    (Signals,signals
    ,getResizeSignal,setResizeSignal
    ,getScrollSignal,setScrollSignal
    ,getPopstateSignal,setPopstateSignal
    ) where

import Ef
import Ef.Event

import Event
import Element

import Unsafe.Coerce
--------------------------------------------------------------------------------
-- Signals

data Signals k
    = Signals
        { resizeSignal :: forall self super. Signal self super SomeEvent
        , resizeSignalSetter :: forall self super. Signal self super SomeEvent -> k

        , scrollSignal :: forall self super. Signal self super SomeEvent
        , scrollSignalSetter :: forall self super. Signal self super SomeEvent -> k

        , popstateSignal :: forall self super. Signal self super SomeEvent
        , popstateSignalSetter :: forall self super. Signal self super SomeEvent -> k
          
        , signalGetter :: k
        }

    | forall self super. GetResizeSignal (Signal self super SomeEvent -> k)
    | forall self super. SetResizeSignal (Signal self super SomeEvent) k

    | forall self super. GetScrollSignal (Signal self super SomeEvent -> k)
    | forall self super. SetScrollSignal (Signal self super SomeEvent) k

    | forall self super. GetPopstateSignal (Signal self super SomeEvent -> k)
    | forall self super. SetPopstateSignal (Signal self super SomeEvent) k

instance Ma Signals Signals where
    ma use Signals{..} (GetResizeSignal sk)    = use signalGetter $ sk resizeSignal
    ma use Signals{..} (GetScrollSignal sk)    = use signalGetter $ sk scrollSignal
    ma use Signals{..} (GetPopstateSignal sk)  = use signalGetter $ sk popstateSignal
    ma use Signals{..} (SetResizeSignal s k)   = use (resizeSignalSetter s) k
    ma use Signals{..} (SetScrollSignal s k)   = use (scrollSignalSetter s) k
    ma use Signals{..} (SetPopstateSignal s k) = use (popstateSignalSetter s) k

signals = Signals {..}
    where

        resizeSignal = undefined
        resizeSignalSetter rs fs =
            let Signals _ rss ss sss pss pssg sg = view fs
            in return $ fs .= Signals (unsafeCoerce rs) rss ss sss pss pssg sg

        scrollSignal = undefined
        scrollSignalSetter ss fs =
            let Signals rs rss _ sss pss pssg sg = view fs
            in return $ fs .= Signals rs rss (unsafeCoerce ss) sss pss pssg sg

        popstateSignal = undefined
        popstateSignalSetter pss fs =
            let Signals rs rss ss sss _ pssg sg = view fs
            in return $ fs .= Signals rs rss ss sss (unsafeCoerce pss) pssg sg

        signalGetter = return



getResizeSignal = self (GetResizeSignal id)
setResizeSignal s = self (SetResizeSignal s ())

getScrollSignal = self (GetScrollSignal id)
setScrollSignal s = self (SetScrollSignal s ())

getPopstateSignal = self (GetPopstateSignal id)
setPopstateSignal s = self (SetPopstateSignal s ())
                      
