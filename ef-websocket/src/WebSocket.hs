{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language RecordWildCards #-}
{-# language TypeOperators #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
module WebSocket (WebSocket, WebSocket.makeListenSocket, ws, S.accept, wsClose, send, receive, Nat, Proxy(..)) where

import Ef

import Control.Exception
import Data.Proxy
import GHC.TypeLits

import qualified Network.Socket as S

import qualified Network.WebSockets as WS

import Data.ByteString.Lazy.Char8

data WebSocket (port :: kind) k
  = WebSocket
    { webSocket :: (Maybe WS.Connection,k)
    , webSocketSetter :: Maybe WS.Connection -> k
    }
  | GetWebSocket (Maybe WS.Connection -> k)
  | SetWebSocket (Maybe WS.Connection) k

instance Ma (WebSocket port) (WebSocket port) where
  ma use WebSocket {..} (GetWebSocket ck) = ma use webSocket ck
  ma use WebSocket {..} (SetWebSocket c k) = ma use webSocketSetter (c,k)

ws :: forall traits super super' port.
      (Monad super, Lift IO super, Monad super', Lift IO super', KnownNat port, '[WebSocket port] .> traits)
   => Proxy port -> S.Socket -> super' (Trait (WebSocket port) traits super)
ws _ listenSocket = do
  c <- lift $ WS.makePendingConnection listenSocket WS.defaultConnectionOptions >>= WS.acceptRequest
  return $ WebSocket
    { webSocket = (Just c,return)
    , webSocketSetter = \c fs -> do
        let ws = view fs :: WebSocket port (Method traits super)
            (_,wsGetter) = webSocket ws
        return $ fs .= (ws { webSocket = (c,wsGetter) } :: WebSocket port (Method traits super))
    }

getWS :: forall self super port.
         (Monad super, Lift IO super, '[WebSocket port] :> self)
      => Proxy port -> Narrative self super (Maybe WS.Connection)
getWS _ = self (GetWebSocket id :: WebSocket port (Maybe WS.Connection))

wsClose :: forall self super port.
           (Monad super, Lift IO super, '[WebSocket port] :> self)
        => Proxy port -> Narrative self super ()
wsClose p = do
  mc <- getWS p
  case mc of
    Just c -> do
      lift $ WS.sendClose c (pack $ show ())
      self (SetWebSocket Nothing () :: WebSocket port ())
    Nothing -> return ()

data WSException = WSClosed | WSBadMessage deriving (Show)
instance Exception WSException

send :: forall self super port a.
        (Monad super, Show a, Lift IO super, '[WebSocket port] :> self)
     => Proxy port -> a -> Narrative self super ()
send p a = do
  mc <- getWS p
  case mc of
    Just c -> do
      let bs = pack $ show a
          msg = WS.Text bs
      lift $ WS.sendDataMessage c msg
    Nothing -> Ef.throw WSClosed

receive :: forall self super port.
           (Monad super, Lift IO super, '[WebSocket port] :> self)
        => Proxy port -> Narrative self super String
receive p = do
  mc <- getWS p
  case mc of
    Just c -> do
      m <- lift $ WS.receive c
      case m of
        WS.DataMessage dm ->
          case dm of
            WS.Text t ->
              return $ unpack t
            _ ->
              Ef.throw WSBadMessage
        _ -> Ef.throw WSBadMessage
    Nothing -> Ef.throw WSClosed

makeListenSocket :: forall super port.
                    (Monad super, Lift IO super, KnownNat port)
                 => String -> Proxy port -> super S.Socket
makeListenSocket hn _ = lift $ WS.makeListenSocket hn (fromInteger $ natVal (Proxy :: Proxy port))
