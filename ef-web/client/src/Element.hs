{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Event as Event
import qualified GHCJS.DOM.EventTargetClosures as Event
import qualified GHCJS.DOM.EventTarget as Event
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.Element as Elem
import qualified GHCJS.DOM.Types as Types
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM as DOM

import qualified Data.Sequence as Seq
import qualified Control.Monad.Trans.Class as Trans

import Data.Typeable
import Data.Maybe
import Unsafe.Coerce

data Element self super = Element
    { tag :: String
    , identity :: Maybe String
    , dom :: Elem.Element
    , children :: Seq.Seq (Element self super)
    , inbound :: React.Signal self super SomeEvent
    , outbound :: React.Signal self super SomeEvent
    }

data HTML (self :: [* -> *]) (super :: * -> *) k where
    Child :: String
          -> Narrative '[HTML self super] (Narrative self super) a
          -> ((Element self super,a) -> k)
          -> HTML self super k

    Listen :: (Types.IsEvent e,Typeable e)
           => Event.EventName Elem.Element e
           -> ((React.Signal self super e,Narrative self super ()) -> k)
           -> HTML self super k

    SetAttribute
        :: String
        -> String
        -> k
        -> HTML self super k

    AddClass
        :: String
        -> k
        -> HTML self super k

    RemoveClass
        :: String
        -> k
        -> HTML self super k

    SetStyle
        :: String
        -> String
        -> k
        -> HTML self super k

    RemoveStyle
        :: String
        -> k
        -> HTML self super k

data AddAttr = AddAttr String String

data Layout (self :: [* -> *]) (super :: * -> *) k = Layout k

layout = Layout return

instance Ma (Layout (self :: [* -> *]) (super :: * -> *)) (HTML self super)

embed :: (Node.IsNode e, Lift IO super, Monad super)
      => e -> Element self super -> Narrative self super ()
embed e el = do
    lift (Node.appendChild e (Just $ Types.toNode $ dom el) :: IO (Maybe Node.Node))
    return ()

-- infixr 5 #
child :: forall self super a.
       Monad super
    => String
    -> Narrative '[HTML self super] (Narrative self super) a
    -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
child tagName f = self (Child tagName f id)

listen en = self (Listen en id)

setAttr :: forall self super.
           Monad super
        => String -> String -> Narrative '[HTML self super] (Narrative self super) ()
setAttr attr val = self (SetAttribute attr val () :: HTML self super ())

addClass :: forall self super.
            Monad super
         => String -> Narrative '[HTML self super] (Narrative self super) ()
addClass cls = self (AddClass cls () :: HTML self super ())

removeClass :: forall self super.
               Monad super
            => String -> Narrative '[HTML self super] (Narrative self super) ()
removeClass cls = self (RemoveClass cls () :: HTML self super ())

removeStyle :: forall self super.
               Monad super
            => String -> Narrative '[HTML self super] (Narrative self super) ()
removeStyle st = self (RemoveStyle st () :: HTML self super ())

(=:) :: forall self super.
        Monad super
     => String -> String -> Narrative '[HTML self super] (Narrative self super) ()
(=:) k v = self (SetStyle k v () :: HTML self super ())


(+#) :: (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
     => Element self super -> String -> Narrative self super (Element self super)
(+#) e cl = fst <$> (with e $ Element.addClass cl)

(-#) :: (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
     => Element self super -> String -> Narrative self super (Element self super)
(-#) e cl = fst <$> (with e $ Element.removeClass cl)

(-:) :: (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
     => Element self super -> String -> Narrative self super (Element self super)
(-:) e st = fst <$> (with e $ Element.removeStyle st)

with :: forall self super a.
        (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
     => Element self super
     -> Narrative '[HTML self super] (Narrative self super) a
     -> Narrative self super (Element self super,a)
with rootElement f = do
    ev <- React.event return
    Signaled buf <- getSignaled
    doc <- getDocument
    (_,a) <- (Object $ layout *:* Empty) $.
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
                                    Child tagName sub eak -> do
                                        Just el <- io $ Document.createElement doc (Just tagName)
                                        i <- React.construct undefined
                                        o <- React.construct undefined
                                        let e = Element tagName Nothing el Seq.empty i o
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
                                    SetAttribute attr val k -> do
                                        super (Web.setAttr (dom currentElement) attr val)
                                        go (rest k)
                                    AddClass cls k -> do
                                        super (Web.addClass (dom currentElement) cls)
                                        go (rest k)
                                    RemoveClass cls k -> do
                                        super (Web.removeClass (dom currentElement) cls)
                                        go (rest k)
                                    SetStyle key val k -> do
                                        super (Web.setStyle (dom currentElement) key val)
                                        go (rest k)
                                    RemoveStyle key k -> do
                                        super (Web.removeStyle (dom currentElement) key)
                                        go (rest k)

html :: forall self super a.
        (Lift IO super, Monad super, '[Web,SingleKnot] <: self)
     => String
     -> Narrative '[HTML self super] (Narrative self super) a
     -> Narrative self super (Element self super,a)
html tag f = do
    ev <- React.event return
    Signaled buf <- getSignaled
    doc <- getDocument
    fmap snd $ Object (layout *:* Empty) $. do
        Just el <- lift (Document.createElement doc (Just tag) :: IO (Maybe Elem.Element))
        i <- React.construct undefined
        o <- React.construct undefined
        let e = Element tag Nothing el Seq.empty i o :: Element self super
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
                                    Child tagName sub eak -> do
                                        Just el <- io $ Document.createElement doc (Just tagName)
                                        i <- React.construct undefined
                                        o <- React.construct undefined
                                        let e = Element tagName Nothing el Seq.empty i o
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
                                    SetAttribute attr val k -> do
                                        lift (Elem.setAttribute (dom currentElement) attr val :: IO ())
                                        go (rest k)
                                    AddClass cls k -> do
                                        lift (Web.rawAddClass (dom currentElement) cls)
                                        go (rest k)
                                    RemoveClass cls k -> do
                                        lift (Web.rawRemoveClass (dom currentElement) cls)
                                        go (rest k)
                                    SetStyle key val k -> do
                                        lift (Web.rawSetStyle (dom currentElement) key val)
                                        go (rest k)
                                    RemoveStyle key k -> do
                                        lift (Web.rawRemoveStyle (dom currentElement) key)
                                        go (rest k)
