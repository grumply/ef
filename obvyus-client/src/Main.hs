{-# language OverloadedStrings #-}
module Main where

import Ef

import Iron

import Carbon as CSS
import Helium
import Hydrogen as HTML
import Oxygen
import Magnesium
import Silicon
import Attributes as A
import Elements as E

import WebSocket
import Flex

import Control.Monad

import JavaScript.Web.MessageEvent

server = Proxy :: Proxy 8080

main = run Config{..}
  where

    prime base = return $ ws server *:* base

    build = do
      wsConnect server
      onMessage server $ \msg -> do
        case msg of
          StringData jss -> lift $ print jss
        return True
      void $ with fusion $
        style $ height =: per 100
      styleGlobal "*" $ do
        margin =: auto
        padding =: zero
      styleGlobal (commas <| str (string "html") (string "body")) $
        height =: per 100

    routes = dispatch $
      return ()
