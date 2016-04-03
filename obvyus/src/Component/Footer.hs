{-# language RecordWildCards #-}
module Component.Footer where

import Carbon hiding (text)
import qualified Hydrogen as HTML
import Hydrogen
import Oxygen
import Iron

import Flex

import Component.Util

import App

import Control.Monad
import Prelude hiding (span)

footer :: Component ()
footer = Atom {..}
  where

    tag = HTML.footer

    styles =
      Flex.row

    element = do
      embed bottomLinkContainer
      embed copyrightContainer

bottomLinkContainer :: Component ()
bottomLinkContainer = Atom {..}
  where

    tag = division

    styles = do
      Flex.center
      Flex.col 100

    element = do
      embed bottomLinks

copyrightContainer :: Component ()
copyrightContainer = Atom {..}
  where

    tag = division

    styles = do
      Flex.center
      Flex.col 100

    element = do
      embed copyright

bottomLinks :: Component ()
bottomLinks = Atom {..}
  where

    tag = unordered

    styles = do
      listStyle =: none
      margin    =: zero
      padding   =: zero

    element = do
      embed $ bottomLinkBox "About" "/about"
      embed bottomSeparator
      embed $ bottomLinkBox "Contact" "/contact"
      embed bottomSeparator
      embed $ bottomLinkBox "Privacy" "/privacy"

bottomLinkBox :: String -> String -> Component ()
bottomLinkBox txt lnk = Atom {..}
  where

    tag = item

    styles =
      display =: inline

    element =
      embed (bottomLink txt lnk)

bottomLink :: String -> String -> Component ()
bottomLink txt lnk = Atom {..}
  where

    tag = anchor

    styles = do
      textDecoration =: none
      color          =: gray
      fontSize       =: px 12

    element = void $ do
      href lnk
      text txt

bottomSeparator :: Component ()
bottomSeparator = Atom {..}
  where

    tag = item

    styles = do
      display  =: inline
      color    =: gray
      fontSize =: px 12
      margin   =: px2 0 10

    element = void $
      text "|"

copyright :: Component ()
copyright = Atom {..}
  where

    tag = span

    styles = do
      color    =: gray
      fontSize =: px 12

    element = void $
      text "© 2016 S. M. Hickman"
