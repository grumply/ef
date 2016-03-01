{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Element where

import Ef
import qualified Ef.Event as React
import Ef.IO

import Ef.Single

import Event
import Queue
import Signaled
import Web

import qualified GHCJS.Marshal as Types
import qualified GHCJS.Types as Types
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Event as Event
import qualified GHCJS.DOM.EventTargetClosures as Event
import qualified GHCJS.DOM.EventTarget as Event
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.Element as Elem
import qualified GHCJS.DOM.Types as Types
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM as DOM

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Control.Monad.Trans.Class as Trans

import Control.Monad
import Data.Typeable
import Data.Maybe
import Unsafe.Coerce
import Data.String
import Data.List

import Prelude hiding (log)



-- | Element contains the tag name, identity, a sequence of children, and an inbound
-- signal. The W3C specification for HTML does not specify that an identity should not change, but this approach
-- fixes an identity.
data Element self super = Element
    { tag :: String
    , identity :: Maybe String
    , dom :: Elem.Element
    , children :: Seq.Seq (Element self super)
    , inbound :: React.Signal self super SomeEvent
    }



data HTML (self :: [* -> *]) (super :: * -> *) k where
    Child :: Maybe String
          -> String
          -> Narrative '[HTML self super] (Narrative self super) a
          -> ((Element self super,a) -> k)
          -> HTML self super k

    Listen :: (Types.IsEvent e,Typeable e)
           => Event.EventName Elem.Element e
           -> ((React.Signal self super e,Narrative self super ()) -> k)
           -> HTML self super k

    Intercept :: (Types.IsEvent e,Typeable e)
              => Event.EventName Elem.Element e
              -> ((React.Signal self super e,Narrative self super ()) -> k)
              -> HTML self super k

    SetAttribute    :: String -> String -> k -> HTML self super k
    RemoveAttribute :: String -> k -> HTML self super k

    AddClass        :: String -> k -> HTML self super k
    SetClass        :: String -> k -> HTML self super k
    RemoveClass     :: String -> k -> HTML self super k

    SetStyle        :: String -> String -> k -> HTML self super k
    RemoveStyle     :: String -> k -> HTML self super k

    SetText         :: String -> k -> HTML self super k
    RemoveText      :: k -> HTML self super k

    GetElement      :: (Element self super -> k) -> HTML self super k



data Elements (self :: [* -> *]) (super :: * -> *) k
    = Elements k



elements = Elements return



instance Ma (Elements (self :: [* -> *]) (super :: * -> *)) (HTML self super)



(>:) :: (Node.IsNode e, Lift IO super, Monad super)
      => e -> Element self super -> Narrative self super ()
(>:) e el = do
    lift (Node.appendChild e (Just $ Types.toNode $ dom el) :: IO (Maybe Node.Node))
    return ()



getBody = do
    doc <- getDocument
    Just body <- io $ Document.getBody doc
    return body



appendBody child = do
    body <- getBody
    body >: child



replaceBody old new = do
    body <- getBody
    io $ Node.replaceChild body (Just $ dom new) (Just $ dom old)



-- | Create a new Element as the child of the current Element. This will
-- add the child Element to the current Element's list of children as append
-- the new child's DOM node to the current Element's DOM node.
child :: forall self super a. Monad super
      => Maybe String
      -> String
      -> Narrative '[HTML self super] (Narrative self super) a
      -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
child mId tagName f = self (Child mId tagName f id)



-- | Listen to a given type of event for the current Element. This will
-- not cause a preventDefault or stopPropagation.
listen en = self (Listen en id)



-- | Intercept a given type of event for the current Element. This will
-- cause a preventDefault and stopPropagation.
intercept en = self (Intercept en id)



-- | Add an attribute to the current element. As with the other attribute-
-- oriented methods, this is discouraged as it goes against the grain for this
-- development style; use functions to apply groups of styles and signals to
-- modify them dynamically.
setAttr :: forall self super. Monad super
        => String -> String -> Narrative '[HTML self super] (Narrative self super) ()
setAttr attr val = self (SetAttribute attr val () :: HTML self super ())



-- | Add an attribute to the current Element. As with the other attribute-
-- oriented methods, this is discouraged as it goes against the grain for this
-- development style; use functions to apply groups of styles and signals to
-- modify them dynamically.
removeAttr :: forall self super. Monad super
           => String -> Narrative '[HTML self super] (Narrative self super) ()
removeAttr attr = self (RemoveAttribute attr () :: HTML self super ())



-- | Add a class to the current Element. As with the other attribute-oriented
-- methods, this is discouraged as it goes against the grain for this
-- development style; use functions to apply groups of styles and signals to
-- modify them dynamically.
addClass :: forall self super. Monad super
         => String -> Narrative '[HTML self super] (Narrative self super) ()
addClass cls = self (AddClass cls () :: HTML self super ())



-- | Remove the given class from the current Element. As with the other
-- attribute-oriented methods, this is discouraged as it goes against the grain
-- for this development style; use functions to apply groups of styles and
-- signals to modify them dynamically.
removeClass :: forall self super. Monad super
            => String -> Narrative '[HTML self super] (Narrative self super) ()
removeClass cls = self (RemoveClass cls () :: HTML self super ())



-- | Set the class attribute to the given string. This overwrites all existing
-- classes. As with the other attribute-oriented methods, this is discouraged
-- as it goes against the grain for this development style; use functions to
-- apply groups of styles and signals to modify them dynamically.
setClass :: forall self super. Monad super
         => String -> Narrative '[HTML self super] (Narrative self super) ()
setClass st = self (SetClass st () :: HTML self super ())



-- | Set a style for the current Element. This will overwrite any existing
-- value for the given style.
infixr 5 =:
(=:) :: forall self super.
        Monad super
     => String -> String -> Narrative '[HTML self super] (Narrative self super) ()
(=:) style value = self (SetStyle style value () :: HTML self super ())
setStyle = (=:)



-- | Remove a given style from the current Element.
removeStyle :: forall self super. Monad super
            => String -> Narrative '[HTML self super] (Narrative self super) ()
removeStyle style = self (RemoveStyle style () :: HTML self super ())



-- | Set the innerText of the current element.
setText :: forall self super. Monad super
        => String -> Narrative '[HTML self super] (Narrative self super) ()
setText str = self (SetText str () :: HTML self super ())



-- | Remove the innerText form the current element.
removeText :: forall self super. Monad super
           => Narrative '[HTML self super] (Narrative self super) ()
removeText = self (RemoveText () :: HTML self super ())



-- | Get the `Element` the current block is modifying, like a version of `this`.
element :: Monad super
        => Narrative '[HTML self super] (Narrative self super) (Element self super)
element = self (GetElement id)



--------------------------------------------------------------------------------
-- Unsafe combinators for direct modification of Elements bypassing the rAF
-- loop.



-- | Add a class to the given Element. This bypasses the rAF loop and may cause
-- a Reflow/Repaint event. As with all class-oriented methods, this method is
-- discouraged as it goes against the grain of the intended Element-oriented
-- development style. A better approach is to use functions to apply style
-- groups, and signals to modify them dynamically.
(+#) :: (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
     => Element self super
     -> String
     -> Narrative self super (Element self super)
(+#) e cl = fst <$> (with e $ Element.addClass cl)



-- | Remove a class from the given Element. This bypasses the rAF loop and may
-- cause Reflow/Repaint event. As with all class-oriented methods, this method
-- is discouraged as it goes against the grain of the intended Element-oriented
-- development style. A better aproach is to use functions to apply style
-- groups, and signals to modify them dynamically.
(-#) :: (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
     => Element self super
     -> String
     -> Narrative self super (Element self super)
(-#) e cl = fst <$> (with e $ Element.removeClass cl)



-- | Remove a style from the given Element. This bypasses the rAF loop and may
-- cause Reflow/Repaint event.
(-:) :: (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
     => Element self super
     -> String
     -> Narrative self super (Element self super)
(-:) e st = fst <$> (with e $ Element.removeStyle st)



-- | Apply an `HTML` block to the given Element.
with :: forall self super a.
        (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
     => Element self super
     -> Narrative '[HTML self super] (Narrative self super) a
     -> Narrative self super (Element self super,a)
with rootElement f = do
    ev <- React.event return
    Signaled buf <- getSignaled
    doc <- getDocument
    (_,a) <- (Object $ elements *:* Empty) $.
        manipulate ev doc (unsafeCoerce buf) rootElement f
    return a
    where
      manipulate :: forall self super a.
                    (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
                    => React.Event self super
                 -> Document.Document
                 -> Queue ([SomeEvent],React.Signal self super SomeEvent)
                 -> Element self super
                 -> Narrative '[HTML self super] (Narrative self super) a
                 -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
      manipulate React.Event{..} doc global = withElement where
            withElement :: forall b.
                           Element self super
                        -> Narrative '[HTML self super] (Narrative self super) b
                        -> Narrative '[HTML self super] (Narrative self super) (Element self super,b)
            withElement currentElement = go
                where
                    go (Return result) = Return (currentElement,result)
                    go (Fail e) = Fail e
                    go (Super sup) = Super (fmap go sup)
                    go (Say msg rest) =
                        case prj msg of
                            Just x ->
                                case x of
                                    Child mId tagName sub eak -> do
                                        Just el <- io $ Document.createElement doc (Just tagName)
                                        i <- React.construct undefined
                                        let e = Element tagName mId el Seq.empty i
                                        io $ Node.appendChild (dom currentElement) (Just el)
                                        (child,a) <- withElement e sub
                                        withElement
                                            (currentElement
                                                { children = (children currentElement) Seq.|> child }
                                            )
                                            (rest $ eak (child,a))

                                    Listen (en :: Event.EventName Elem.Element e) k -> do
                                        filtered <- super $ filterSignal (\(SomeEvent ev) ->
                                                                        case cast ev of
                                                                            Just (x :: e) -> True
                                                                            _ -> False
                                                                    ) undefined (inbound currentElement)
                                        mapped <- super $ mapSignal (\(SomeEvent ev) -> (fromJust $ cast ev :: e)) undefined filtered
                                        removeListener <- lift $ EventM.on (dom currentElement) en $ do
                                            e <- EventM.event 
                                            Trans.lift $ arrive global ([toEvent e],inbound currentElement)
                                        go (rest $ k (mapped,lift removeListener))
                                    Intercept (en :: Event.EventName Elem.Element e) k -> do
                                        filtered <- super $ filterSignal (\(SomeEvent ev) ->
                                                                        case cast ev of
                                                                            Just (x :: e) -> True
                                                                            _ -> False
                                                                    ) undefined (inbound currentElement)
                                        mapped <- super $ mapSignal (\(SomeEvent ev) -> (fromJust $ cast ev :: e)) undefined filtered
                                        removeListener <- lift $ EventM.on (dom currentElement) en $ do
                                            EventM.preventDefault
                                            EventM.stopPropagation
                                            e <- EventM.event
                                            Trans.lift $ arrive global ([toEvent e],inbound currentElement)
                                        go (rest $ k (mapped,lift removeListener))


                                    SetAttribute attr val k -> do
                                        super (Web.setAttr (dom currentElement) attr val)
                                        go (rest k)
                                    RemoveAttribute attr k -> do
                                        super (Web.setAttr (dom currentElement) attr "")
                                        go (rest k)

                                    AddClass cls k -> do
                                        super (Web.addClass (dom currentElement) cls)
                                        go (rest k)
                                    RemoveClass cls k -> do
                                        super (Web.removeClass (dom currentElement) cls)
                                        go (rest k)
                                    SetClass cls k -> do
                                        super (Web.setClass (dom currentElement) cls)
                                        go (rest k)

                                    SetStyle key val k -> do
                                        super (Web.setStyle (dom currentElement) key val)
                                        go (rest k)
                                    RemoveStyle key k -> do
                                        super (Web.removeStyle (dom currentElement) key)
                                        go (rest k)

                                    SetText str k -> do
                                        super (Web.setText (dom currentElement) str)
                                        go (rest k)
                                    RemoveText k -> do
                                        super (Web.setText (dom currentElement) "")
                                        go (rest k)

                                    GetElement ek -> go (rest $ ek currentElement)



html ::  forall self super a.
        (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
     => Maybe String
     -> String
     -> Narrative '[HTML self super] (Narrative self super) ()
     -> Narrative self super (Element self super)
html mId tag f = fst <$> create mId tag f



create :: forall self super a.
        (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
     => Maybe String
     -> String
     -> Narrative '[HTML self super] (Narrative self super) a
     -> Narrative self super (Element self super,a)
create mId tag f = do
    ev <- React.event return
    Signaled buf <- getSignaled
    doc <- getDocument
    fmap snd $ Object (elements *:* Empty) $. do
        Just el <- lift (Document.createElement doc (Just tag) :: IO (Maybe Elem.Element))
        i <- React.construct undefined
        let e = Element tag mId el Seq.empty i :: Element self super
        manipulate ev doc (unsafeCoerce buf) e f
    where
      manipulate :: forall self super b.
                    (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
                 => React.Event self super
                 -> Document.Document
                 -> Queue ([SomeEvent],React.Signal self super SomeEvent)
                 -> Element self super
                 -> Narrative '[HTML self super] (Narrative self super) b
                 -> Narrative '[HTML self super] (Narrative self super) (Element self super,b)
      manipulate React.Event{..} doc global = withElement where
            withElement :: forall b.
                           Element self super
                        -> Narrative '[HTML self super] (Narrative self super) b
                        -> Narrative '[HTML self super] (Narrative self super) (Element self super,b)
            withElement currentElement = go
                where
                    go (Return result) = Return (currentElement,result)
                    go (Fail e) = Fail e
                    go (Super sup) = Super (fmap go sup)
                    go (Say msg rest) =
                        case prj msg of
                            Just x ->
                                case x of
                                    Child mId tagName sub eak -> do
                                        Just el <- io $ Document.createElement doc (Just tagName)
                                        i <- React.construct undefined
                                        let e = Element tagName mId el Seq.empty i
                                        io $ Node.appendChild (dom currentElement) (Just el)
                                        (child,a) <- withElement e sub
                                        withElement
                                            (currentElement
                                                { children = (children currentElement) Seq.|> child }
                                            )
                                            (rest $ eak (child,a))
                                    Listen (en :: Event.EventName Elem.Element e) k -> do
                                        filtered <- super $ filterSignal (\(SomeEvent ev) ->
                                                                        case cast ev of
                                                                            Just (x :: e) -> True
                                                                            _ -> False
                                                                    ) undefined (inbound currentElement)
                                        mapped <- super $ mapSignal (\(SomeEvent ev) -> (fromJust $ cast ev :: e)) undefined filtered
                                        removeListener <- lift $ EventM.on (dom currentElement) en $ do
                                            e <- EventM.event 
                                            Trans.lift $ arrive global ([toEvent e],inbound currentElement)
                                        go (rest $ k (mapped,lift removeListener))
                                    Intercept (en :: Event.EventName Elem.Element e) k -> do
                                        filtered <- super $ filterSignal (\(SomeEvent ev) ->
                                                                        case cast ev of
                                                                            Just (x :: e) -> True
                                                                            _ -> False
                                                                    ) undefined (inbound currentElement)
                                        mapped <- super $ mapSignal (\(SomeEvent ev) -> (fromJust $ cast ev :: e)) undefined filtered
                                        removeListener <- lift $ EventM.on (dom currentElement) en $ do
                                            EventM.preventDefault
                                            EventM.stopPropagation
                                            e <- EventM.event 
                                            Trans.lift $ arrive global ([toEvent e],inbound currentElement)
                                        go (rest $ k (mapped,lift removeListener))

                                    SetAttribute attr val k -> do
                                        lift (Elem.setAttribute (dom currentElement) attr val :: IO ())
                                        go (rest k)
                                    RemoveAttribute attr k -> do
                                        lift (Elem.setAttribute (dom currentElement) attr "" :: IO ())
                                        go (rest k)

                                    AddClass cls k -> do
                                        lift (Web.rawAddClass (dom currentElement) cls)
                                        go (rest k)
                                    RemoveClass cls k -> do
                                        lift (Web.rawRemoveClass (dom currentElement) cls)
                                        go (rest k)
                                    SetClass cls k -> do
                                        lift (Web.rawSetClass (dom currentElement) cls)
                                        go (rest k)

                                    SetStyle key val k -> do
                                        lift (Web.rawSetStyle (dom currentElement) key val)
                                        go (rest k)
                                    RemoveStyle key k -> do
                                        lift (Web.rawRemoveStyle (dom currentElement) key)
                                        go (rest k)

                                    SetText str k -> do
                                        lift (Web.rawSetText (dom currentElement) str)
                                        go (rest k)
                                    RemoveText k -> do
                                        lift (Web.rawSetText (dom currentElement) "")
                                        go (rest k)

                                    GetElement ek -> go (rest $ ek currentElement)
