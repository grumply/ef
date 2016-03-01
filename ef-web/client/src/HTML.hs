{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module HTML
    ( Element(..), Blocking(..), Client(..)
    , SomeEvent(..), Event(..)
    , React.event, React.Signal
    , html, with, (>:), child, listen, intercept 
    , (=:), (+#), (-#), (-:)
    , getScreen, getWindow, getDrawer, getDocument
    , Element.getBody, Element.appendBody
    , client, simpleClient
    , Element.HTML
    , on
    , Element.addClass, Element.setClass, Element.setAttr, Element.setText
    , Element.removeStyle
    , getWindowWidth
    , element
    , reactiveDependentFeature
    , Web.query, Web.write, arrive
    , EfClient
    , minWidth, maxWidth, betweenWidth
    , module Router
    , module Document
    , module Signals
    , module Data.Promise
    ) where

import Ef
import Ef.IO
import Ef.Single hiding (Client)
import Ef.Fiber

import Data.Promise

import qualified Ef.Event as React
import qualified Ef.Knot.Methods.Single as Method
import qualified Ef.Fiber.Methods as Method

import Element
import Event
import Router 
import Document
import Signaled
import Signals
import Queue
import Web
import qualified Web.Methods as Method

import qualified GHCJS.DOM as DOM
import qualified GHCJS.Types as Types
import qualified GHCJS.DOM.Types as Types
import qualified GHCJS.Marshal as Types
import qualified GHCJS.Foreign as Types
import qualified GHCJS.DOM.PopStateEvent as PSE
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.Element as DOMElement
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Location as Location
import qualified Control.Monad.Trans.Class as Trans
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.Document as DOMDocument
import qualified GHCJS.DOM.Node as Node

import Data.Coerce
import qualified JavaScript.Object as JSO
import qualified JavaScript.Object.Internal as JSO

import Control.Monad
import Unsafe.Coerce
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

data Blocking = Blocking | Nonblocking deriving Eq

type EfClient = '[Router,Signals,Web,SingleKnot]

simpleClient fof f = client $ Client (\x -> return (x,())) (const f) go
    where
        go intcptr _ = start
            where
                start obj = do
                    (obj',_) <- obj $. intcptr Blocking
                    start obj'

data Client self methods super primeResult buildResult = Client
    { prime :: Methods '[Documents,Router,Signals,Method.Web,Method.SingleKnot] (Implementation methods super)
                -> super (Methods methods (Implementation methods super),primeResult)
    , build :: primeResult -> Narrative self super buildResult
    , drive :: (Blocking -> Narrative self super ()) -> buildResult -> Object methods super -> super ()
    }

foreign import javascript unsafe
    "console.log($1);"
    clog :: Types.JSVal -> IO ()


client :: forall self super methods primeResult buildResult.
          ( Ma (Methods methods) (Messages self)
          , Subclass '[Documents,Router,Signals,Method.Web,Method.SingleKnot] methods
          , '[Documents,Router,Signals,Web,SingleKnot] <: self
          , Lift IO super
          , Monad super
          )
       => Client self methods super primeResult buildResult -> super ()
client Client{..} = do
    env <- lift Method.createWeb
    Just win <- lift DOM.currentWindow
    Just loc <- lift (Window.getLocation win :: IO (Maybe Location.Location))
    Just doc <- lift (Window.getDocument win :: IO (Maybe DOMDocument.Document))
    Just body <- lift (DOMDocument.getBody doc :: IO (Maybe HTMLElement.HTMLElement))
    Just seed <- lift (DOMDocument.createElement doc (Just "ef") :: IO (Maybe DOMElement.Element))
    lift (Node.appendChild body (Just seed) :: IO (Maybe Node.Node))
    let seedDoc = Document (Element "ef" Nothing seed Seq.empty undefined) "Ef" "" Map.empty Transient
    let root = documents seedDoc defaultCacheConf *:* routing *:* signals *:* env *:* Method.singleKnot *:* Empty
    (obj,a) <- prime root
    (obj',b) <- Object obj $. do
                    rs <- resizeSignal
                    setResizeSignal rs
                    ss <- scrollSignal
                    setScrollSignal ss
                    ps <- popstateSignal
                    setPopstateSignal ps
                    React.Event{..} <- React.event return
                    behavior ps $ \_ ev -> do
                        case fromEvent ev of
                            Just (e :: PSE.PopStateEvent) -> do
                                url <- io (Location.getHash loc)
                                route url
                            _ -> return ()
                    build a
    (_,buf) <- obj' $. do
        Signaled buf <- getSignaled
        return $ unsafeCoerce buf
    drive (intercepter buf) b obj'
    where
       intercepter globalBuffer = go
           where
               go b = do
                   when (b == Nonblocking) (lift $ arrive globalBuffer ([],undefined))
                   evss <- lift (collect globalBuffer)
                   React.event $ \event ->
                       forM_ evss $ \(ev,s) -> forM_ ev (React.trigger event s)


resizeSignal :: ('[Web,SingleKnot] <: self, Lift IO super, Monad super)
                => Narrative self super (React.Signal self super SomeEvent)
resizeSignal = React.event $ \React.Event{..} -> do
    resizeSignal <- React.construct undefined
    window <- getWindow
    Signaled buf <- getSignaled
    lift $ EventM.on window Window.resize $ do
        e <- EventM.event
        Trans.lift $ arrive (unsafeCoerce buf) ([toEvent e],resizeSignal)
    return resizeSignal

scrollSignal :: ('[Web,SingleKnot] <: self, Lift IO super, Monad super)
                => Narrative self super (React.Signal self super SomeEvent)
scrollSignal = React.event $ \React.Event{..} -> do
    scrollSignal <- React.construct undefined
    window <- getWindow
    Signaled buf <- getSignaled
    lift $ EventM.on window Window.scrollEvent $ do
        e <- EventM.event
        Trans.lift $ arrive (unsafeCoerce buf) ([toEvent e],scrollSignal)
    return scrollSignal

popstateSignal :: ('[Web,SingleKnot] <: self, Lift IO super, Monad super)
               => Narrative self super (React.Signal self super SomeEvent)
popstateSignal = React.event $ \React.Event{..} -> do
    popstateSignal <- React.construct undefined
    window <- getWindow
    Just h <- io $ Window.getHistory window
    Signaled buf <- getSignaled
    lift $ EventM.on window Window.popState $ do
        e <- EventM.event
        Trans.lift $ arrive (unsafeCoerce buf) ([toEvent e],popstateSignal)
    return popstateSignal

getWindowWidth :: ('[Web] <: self, Lift IO super, Monad super)
               => Narrative self super Int
getWindowWidth = do
    window <- getWindow
    lift (Window.getInnerWidth window :: IO Int)

data Applied
    = Applied
    | Unapplied
    deriving Eq

-- create a dynamic set of styles, attributes, and classes
-- that are dependent upon a conditional and that, if applied, will
-- be unapplied upon that conditional failing. Removables specifies
-- what should be unapplied. The conditional evaluation is triggered
-- by a given signal.
reactiveDependentFeature
      :: forall self super a.
         ('[Web,SingleKnot] <: self, Lift IO super, Monad super)
      => Narrative self super a
      -> React.Signal self super SomeEvent
      -> SomeEvent
      -> (a -> SomeEvent -> Bool)
      -> Narrative '[HTML self super] (Narrative self super) ()
      -> Narrative '[HTML self super] (Narrative self super) ()
reactiveDependentFeature x signal initial q styles = do
    e :: Element self super <- element
    super $ do
        (ev,i) <- (,) <$> (React.event return) <*> x
        if q i initial then do
            with e styles
            React.behavior ev signal (withElement ev Applied e)
        else
            React.behavior ev signal (withElement ev Unapplied e)
    where
        withElement React.Event{..} apd e React.Reactor{..} ev =
            case apd of
                Applied -> become (go Applied)
                Unapplied -> do
                    i <- x
                    if q i ev
                        then do with e styles
                                become (go Applied)
                        else become (go Unapplied)
            where
                go Applied ev = do
                    i <- x
                    when (not $ q i ev) $
                        become (go Unapplied)

                go Unapplied ev = do
                    i <- x
                    when (q i ev) $ do
                        with e styles
                        become (go Applied)

on signal f = super $ do
    React.Event{..} <- React.event return
    behavior signal $ const f

minWidth mn f = do
    resizes <- super getResizeSignal
    reactiveDependentFeature getWindowWidth resizes undefined (\w _ -> w >= mn) f

maxWidth mx f = do
    resizes <- super getResizeSignal
    reactiveDependentFeature getWindowWidth resizes undefined (\w _ -> w <= mx) f

betweenWidth mn mx f = do
    resizes <- super getResizeSignal
    reactiveDependentFeature getWindowWidth resizes undefined (\w _ -> w >= mn && w <= mx) f
