{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Router where

import Ef
import Ef.IO
import HTML

import Ef.Event

import GHCJS.Marshal
import GHCJS.Types
import qualified GHCJS.DOM.History as H
import qualified GHCJS.DOM.EventM as E
import qualified GHCJS.DOM.Window as W
import qualified GHCJS.DOM.Element as E

import qualified Control.Monad.Trans.Class as Trans

import Unsafe.Coerce

foreign import javascript unsafe
    "console.log($1);"
    clog :: JSVal -> IO ()



pushUrl h url = io $ do
  dat <- toJSVal (0 :: Int)
  H.pushState h dat "" url

routed = client Client{..}
    where

        prime root = return
            (signals *:* routing *:* root,())
            
        build _ = do
            rs <- HTML.resizeSignal
            ss <- HTML.scrollSignal
            setResizeSignal rs
            setScrollSignal ss
            win <- getWindow
            Just h <- io $ W.getHistory win
            lift $ E.on win W.popState $ do
                ev <- E.event
                E.preventDefault
                E.stopPropagation
                Trans.lift $ print "Fire popstate"
                Trans.lift $ clog =<< toJSVal ev
            Event{..} <- event return
            (content,(sub1clicks,sub2clicks)) <- html "div" $ do
                (_,(sub1Clicks,_)) <- child "a" $ do
                    setText "sub1"
                    setAttr "href" "#/sub1"
                    intercept E.click
                (_,(sub2Clicks,_)) <- child "a" $ do
                    setText "sub2"
                    setAttr "href" "#/sub2"
                    intercept E.click
                return (sub1Clicks,sub2Clicks)
            behavior sub1clicks $ \_ _ ->
                pushUrl h "#/sub1"
            behavior sub2clicks $ \_ _ ->
                pushUrl h "#/sub2"
            appendBody content
            
        drive intercepter _ = run
            where

                run obj = do
                    (obj',_) <- obj $. intercepter Blocking
                    run obj'

--------------------------------------------------------------------------------
-- Router

routing :: Monad super => Use (Router self super) methods super
routing = Router "" undefined return

data Router self super k
    = Router
        { currentRoute :: String
        , history      :: H.History
        , routerGetter :: k
        }

    | CurrentRoute (String -> k)

    | GetHistory (H.History -> k)

    | Route String (Narrative self super ())

    | GetParam String (String -> k)
instance Ma (Router self super) (Router self super)



{-
route "/user" $ do
    ...
route "/user/:userId" $ do
    uid <- get "userId"
    ...


-}


--------------------------------------------------------------------------------
-- Signals

data Signals k
    = Signals
        { resizeSignal :: forall self super. Signal self super SomeEvent
        , resizeSignalSetter :: forall self super. Signal self super SomeEvent -> k

        , scrollSignal :: forall self super. Signal self super SomeEvent
        , scrollSignalSetter :: forall self super. Signal self super SomeEvent -> k

        , signalGetter :: k
        }

    | forall self super. GetResizeSignal (Signal self super SomeEvent -> k)
    | forall self super. SetResizeSignal (Signal self super SomeEvent) k

    | forall self super. GetScrollSignal (Signal self super SomeEvent -> k)
    | forall self super. SetScrollSignal (Signal self super SomeEvent) k

instance Ma Signals Signals where
    ma use Signals{..} (GetResizeSignal sk) = use signalGetter $ sk resizeSignal
    ma use Signals{..} (GetScrollSignal sk) = use signalGetter $ sk scrollSignal
    ma use Signals{..} (SetResizeSignal s k) = use (resizeSignalSetter s) k
    ma use Signals{..} (SetScrollSignal s k) = use (scrollSignalSetter s) k

signals = Signals {..}
    where

        resizeSignal = undefined
        resizeSignalSetter rs fs =
            let Signals _ rss ss sss sg = view fs
            in return $ fs .= Signals (unsafeCoerce rs) rss ss sss sg

        scrollSignal = undefined
        scrollSignalSetter ss fs =
            let Signals rs rss _ sss sg = view fs
            in return $ fs .= Signals rs rss (unsafeCoerce ss) sss sg

        signalGetter = return

getResizeSignal = self (GetResizeSignal id)
setResizeSignal s = self (SetResizeSignal s ())

getScrollSignal = self (GetScrollSignal id)
setScrollSignal s = self (SetScrollSignal s ())


minWidth mn f = do
    resizes <- super getResizeSignal
    reactiveDependentFeature getWindowWidth resizes undefined (\w _ -> w >= mn) f

maxWidth mx f = do
    resizes <- super getResizeSignal
    reactiveDependentFeature getWindowWidth resizes undefined (\w _ -> w <= mx) f

betweenWidth mn mx f = do
    resizes <- super getResizeSignal
    reactiveDependentFeature getWindowWidth resizes undefined (\w _ -> w >= mn && w <= mx) f
