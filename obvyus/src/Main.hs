{-# language OverloadedStrings #-}
{-# language ImplicitParams #-}
{-# language TemplateHaskell #-}
module Main where

import Ef
import Ef.Bidir
import Data.Promise

import Iron

import Carbon as CSS
import qualified Dicarbon
import Helium
import Hydrogen as HTML
import Oxygen
import Magnesium
import Silicon
import Neon
import Attributes as A
import Elements as E

import WebSocket
import Flex

import Control.Monad

import qualified GHCJS.DOM.Element as E

import Prelude hiding (span)

import Data.Time.Clock

import Control.Concurrent

import Data.JSString
import JavaScript.Web.MessageEvent

server = Proxy :: Proxy 8080

main = run Config{..}
  where

    prime base = return $ ws server *:* base

    build = do
      wsConnect server
      start <- lift getCurrentTime
      onMessage server $ \_ -> return True
      void $ with fusion $ do
        style $ height =: per 100
        html editor
        -- embed secondaryWidget

    routes = dispatch $ do
      onState server $ \_ _ -> send server "test"
      return ()

editor = Named {..}
  where

    name = $(unique)

    tag = division

    styles = do
      Flex.row
      Flex.center
      height =: per 100

    element = do
      html styleBar
      html contentArea

styleBar = Named {..}
  where

    name = $(unique)

    tag = division

    styles = do
      Flex.col 80
      margin =: zero
      height =: px 30

    element = return ()



contentArea = Named {..}
  where

    name = $(unique)

    tag = division

    styles = do
      Flex.col 80
      textAlign =: left
      margin =: zero

    element = do
      set contenteditable true
