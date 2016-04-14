{-# language RecordWildCards #-}
module Component.Header where

import App

import Carbon as CSS hiding (text)
import qualified Hydrogen as HTML
import Hydrogen
import Helium as Str
import Neon as Glyph
import Oxygen
import Magnesium
import Silicon
import Iron

import Flex

import Component.Util

import qualified GHCJS.DOM.Element as E

import Control.Monad
import Prelude hiding (span)

header :: Component (Signal App IO ())
header = Atom {..}
  where

    tag = HTML.header

    styles = do
      Flex.row
      marginBottom =: px 6

    element = do
      embed hatLeft
      embed hatRight

hatLeft :: Component ()
hatLeft = Atom {..}
  where

    tag = division

    styles = do
      Flex.start
      Flex.col 90

    element = do
      embed logo
      embed brand
      embed links

logo :: Component ()
logo = Atom {..}
  where

    tag = anchor

    styles = do
      textDecoration =: none
      bgColor        =: darkcyan
      padding        =: px2 2 5
      marginRight    =: px 10
      timesNewRoman bold white (px 15)

    element = void $ do
      text "O"
      href ""

brand :: Component ()
brand = Atom {..}
  where

    tag = anchor

    styles = do
      textDecoration =: none
      marginRight    =: px 16

    element = do
      href ""
      embed $ linkText "Obvy"
      embed separator
      embed $ linkText "us"

linkText :: String -> Component ()
linkText txt = Atom {..}
  where

    tag = span

    styles =
        helveticaNeue (weight 600) black (px 13)

    element = void $
        text txt

separator :: Component ()
separator = Atom {..}
  where

    tag = span

    styles = do
        helveticaNeue (weight 200) (rgba(0,0,0,0.5)) (px 28)
        position =: relative
        top      =: px 4

    element = void $
        text "|"

links :: Component ()
links = Atom {..}
  where

    tag = unordered

    styles = do
      display   =: inline
      listStyle =: none

    element = do
      embed $ linkBox "Interesting" "/interesting"
      embed $ linkBox "Provacative" "/provacative"

linkBox :: String -> String -> Component ()
linkBox txt lnk = Atom {..}
  where

    tag = item

    styles =
      display =: inline

    element =
      embed (Component.Header.link txt lnk)

link :: String -> String -> Component ()
link txt lnk = Named {..}
  where

    tag = anchor

    name = txt ++ "Link"

    styles = do
      cursor         =: pointer
      textDecoration =: none
      fontSize       =: px 14
      color          =: gray
      marginRight    =: px 12

    element = void $ do
      text txt
      href lnk

hatRight :: Component (Signal App IO ())
hatRight = Atom {..}
  where

    tag = division

    styles = do
      Flex.end
      Flex.col 10

    element = do
      embed loginLink

loginLink :: Component (Signal App IO ())
loginLink = Atom {..}
  where

    tag = anchor

    styles = do
      cursor      =: pointer
      marginRight =: px (-4)

    element = do
      embed loginGlyph
      fst <$> listen E.click (const ()) listenOpts

loginGlyph :: Component ()
loginGlyph = Atom {..}
  where

    tag = span

    styles = do
      top            =: px 8
      position       =: relative
      textDecoration =: none
      fontSize       =: px 20
      color          =: darkcyan

    element = void $
      glyph gLogIn
