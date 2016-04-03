{-# language RecordWildCards #-}
module Component.Body where

import Ef

import App

import Carbon as CSS hiding (text)
import Hydrogen hiding (header,footer)
import Helium as Str
import Neon as Glyph
import Oxygen
import Magnesium
import Silicon
import Iron

import Flex

import Components
import Component.Util
import Component.Divider
import Component.Header
import Component.Footer
import Component.Modal
import Component.Login

import Control.Monad

import qualified GHCJS.DOM.Element as E

mainContentContainer :: Component ()
mainContentContainer = Named {..}
  where

    tag = division

    name = mainContentName

    styles = return ()

    element = return ()

bodyTop :: Component (Signal App IO ())
bodyTop = Atom {..}
  where

    tag = division

    styles = do
      minHeight =: per 100
      height    =: spaces <| str auto important
      height    +: per 100
      margin    =: spaces <| str zero auto (ems $ negate footSize)

    element = do
      loginClicks <- embed header
      embed divider
      embed modal
      modalCloseClicks <- super $ with modalName $ do
        example
        fst <$> listen E.click (const ()) listenOpts
      embed mainContentContainer
      loginModalHandler loginClicks modalCloseClicks
      embed feet
      return loginClicks

example =
 void $ do
      child division Nothing $ do
        style $ do
          bgColor       =: black
          Flex.col 95
        text "test"

feet :: Component ()
feet = Atom {..}
  where

    tag = division

    styles =
      height =: ems footSize

    element = return ()

bodyBottom :: Component ()
bodyBottom = Atom {..}
  where

    tag = division

    styles = do
      Flex.container
      height =: ems footSize

    element = do
      embed divider
      embed footer
