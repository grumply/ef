{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language RecordWildCards #-}
{-# language TypeOperators #-}
{-# language FlexibleContexts #-}
{-# language BangPatterns #-}
{-# language ExistentialQuantification #-}
{-# language MultiParamTypeClasses #-}
module WebSocket (WebSocket, ws, getWS, wsConnect, wsDisconnect, send, onState, onMessage, Nat, Proxy(..)) where

import Ef

import Silicon
import Iron

import qualified GHCJS.DOM as D
import qualified GHCJS.DOM.EventM as E
import qualified GHCJS.DOM.Location as L
import qualified GHCJS.DOM.MessageEvent as ME
import qualified GHCJS.DOM.WebSocket as WS
import qualified GHCJS.DOM.Window as W

import qualified Control.Monad.Trans.Class as T

import GHCJS.Buffer
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import qualified JavaScript.Web.MessageEvent as WME

import Control.Monad
import Data.Coerce
import Data.Proxy
import GHC.TypeLits

import Unsafe.Coerce

data WebSocketState = WSOpened | WSError | WSClosed

data WebSocket (port :: Nat) k
  = WebSocket
    { webSocket :: (WS.WebSocket,k)
    , webSocketSetter :: WS.WebSocket -> k

    , webSocketStates :: forall self super. (Signal self super WebSocketState,k)
    , webSocketStatesSetter :: forall self super. Signal self super WebSocketState -> k

    , webSocketMessages :: forall self super. (Signal self super WME.MessageEventData,k)
    , webSocketMessagesSetter :: forall self super. Signal self super WME.MessageEventData -> k

    , webSocketConnecter :: Signaled -> k

    }
  | GetWebSocket (WS.WebSocket -> k)
  | SetWebSocket WS.WebSocket k

  | forall self super. GetWebSocketStates (Signal self super WebSocketState -> k)
  | forall self super. SetWebSocketStates (Signal self super WebSocketState) k

  | forall self super. GetWebSocketMessages (Signal self super WME.MessageEventData -> k)
  | forall self super. SetWebSocketMessages (Signal self super WME.MessageEventData) k

  | WebSocketConnect Signaled k

instance Ma (WebSocket port) (WebSocket port) where
  ma use WebSocket{..} (GetWebSocket wsk)           = ma use webSocket wsk
  ma use WebSocket{..} (SetWebSocket ws k)          = ma use webSocketSetter (ws,k)
  ma use WebSocket{..} (GetWebSocketStates wssk)    = ma use webSocketStates wssk
  ma use WebSocket{..} (SetWebSocketStates wss k)   = ma use webSocketStatesSetter (wss,k)
  ma use WebSocket{..} (GetWebSocketMessages wsmk)  = ma use webSocketMessages wsmk
  ma use WebSocket{..} (SetWebSocketMessages wsm k) = ma use webSocketMessagesSetter (wsm,k)
  ma use WebSocket{..} (WebSocketConnect s k)       = ma use webSocketConnecter (s,k)

ws :: forall super port traits.
      (KnownNat port, Lift IO super, Monad super, '[WebSocket port] .> traits)
   => Proxy port -> Trait (WebSocket port) traits super
ws _ = WebSocket
    { webSocket = (undefined,return)

    , webSocketSetter = \newws fs ->
        let ws = view fs :: WebSocket port (Object traits super -> super (Object traits super))
            (_,wsGetter) = webSocket ws
        in return $ fs .= (ws { webSocket = (newws,wsGetter) } :: WebSocket port (Method traits super))

    , webSocketStates = (undefined,return)
    , webSocketStatesSetter = \wss fs ->
        let ws = view fs :: WebSocket port (Method traits super)
            (_,wssGetter) = webSocketStates ws
        in return $ fs .= (ws { webSocketStates = (unsafeCoerce wss,wssGetter) } :: WebSocket port (Method traits super))

    , webSocketMessages = (undefined,return)
    , webSocketMessagesSetter = \wsm fs ->
        let ws = view fs :: WebSocket port (Method traits super)
            (_,wsmGetter) = webSocketMessages ws
        in return $ fs .= (ws { webSocketMessages = (unsafeCoerce wsm,wsmGetter) } :: WebSocket port (Method traits super))

    , webSocketConnecter = \(Signaled gb) fs ->
        let ws = view fs :: WebSocket port (Method traits super)
            (_,wsGetter) = webSocket ws
            (_,wssGetter) = webSocketStates ws
            (_,wsmsGetter) = webSocketMessages ws
            port = show $ fromInteger $ natVal (Proxy :: Proxy port)
        in do messagesSignal :: Signal self super WME.MessageEventData <- construct undefined
              statesSignal :: Signal self super WebSocketState <- construct undefined
              sock <- liftIO $ do
                Just win <- D.currentWindow
                Just loc <- W.getLocation win
                hn <- L.getHostname loc
                sock <- WS.newWebSocket ("ws://" ++ hn ++ ':':port) (Just [] :: Maybe [String])
                E.on sock WS.open $ T.lift $ arrive (unsafeCoerce gb) ([unsafeCoerce WSOpened],statesSignal)
                E.on sock WS.closeEvent $ T.lift $ arrive (unsafeCoerce gb) ([unsafeCoerce WSClosed],statesSignal)
                E.on sock WS.error $ T.lift $ arrive (unsafeCoerce gb) ([unsafeCoerce WSError],statesSignal)
                E.on sock WS.message $ do
                  ev <- E.event
                  T.lift $ arrive (unsafeCoerce gb) ([WME.getData $ unsafeCoerce ev :: WME.MessageEventData],messagesSignal)
                return sock
              return $ fs .= (ws
                { webSocket = (sock,wsGetter)
                , webSocketStates = (unsafeCoerce statesSignal,wssGetter)
                , webSocketMessages = (unsafeCoerce messagesSignal,wsmsGetter)
                } :: WebSocket port (Method traits super))
    }

getWS :: forall self super port.
         (Monad super, '[WebSocket port] :> self)
      => Proxy port -> Narrative self super WS.WebSocket
getWS _ = self (GetWebSocket id :: WebSocket port WS.WebSocket)

wsConnect :: forall self super port.
             (Monad super, Web :> self, '[WebSocket port] :> self)
          => Proxy port -> Narrative self super ()
wsConnect _ = do
  sb <- getSignalBuffer
  self (WebSocketConnect sb () :: WebSocket port ())

wsDisconnect :: forall self super port.
                (Monad super, Lift IO super, Web :> self, '[WebSocket port] :> self)
             => Proxy port -> Narrative self super ()
wsDisconnect p = do
  ws <- getWS p
  liftIO (WS.close ws 1000 "wsDisconnect called.")

send :: forall self super port.
        (Monad super, Lift IO super, '[WebSocket port] :> self)
     => Proxy port -> String -> Narrative self super ()
send p dat = do
  ws <- getWS p
  liftIO $ WS.sendString ws dat

onMessage :: forall self super port.
             (Monad super, Lift IO super, Web :> self, '[WebSocket port] :> self)
          => Proxy port -> (WME.MessageEventData -> Narrative self super Bool) -> Narrative self super ()
onMessage _ bhvr = do
  wms <- self (GetWebSocketMessages id :: WebSocket port (Signal self super WME.MessageEventData))
  _ <- behavior' wms $ \Reactor{..} wme -> do
    b <- bhvr wme
    unless  b end
  return ()

onState :: forall self super port.
           (Monad super, Lift IO super, Web :> self, '[WebSocket port] :> self)
        => Proxy port -> (Reactor self super WebSocketState -> WebSocketState -> Narrative self super ()) -> Narrative self super ()
onState _ bhvr = do
  wss <- self (GetWebSocketStates id :: WebSocket port (Signal self super WebSocketState))
  _ <- behavior' wss bhvr
  return ()
