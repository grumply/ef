{-# language RecordWildCards #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language PolyKinds #-}
{-# language OverloadedStrings #-}
{-# language NoMonomorphismRestriction #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
module Main where

import Ef
import Ef.Single
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

import Flex

import Listings

import Utility

import Control.Monad

import qualified GHCJS.DOM.Element as E

import Prelude hiding (span)

import Modal.Entry

app base = listings *:* listings *:* base

type App = '[
              Listings 'ProvocativeListings,
              Listings 'InterestingListings,
             -- Obvyus,
             -- Menu,
             Oxygen,
             Silicon,
             SingleKnot
            ]

type CSS = Narrative '[Carbon] IO
type HTML = Narrative '[Hydrogen] (Narrative App IO)
type Route = Narrative '[Magnesium] (Narrative App IO)

type Component = Atom App IO

data Model k
  = Model
        {
        }

defaultRouter =  do
  path "/interesting" $
    interesting $
      return ()

  path "/about" $
    about $
      return ()

  path "/contact" $
    contact $
      return ()

  path "/privacy" $
    privacy $
      return ()

  provacative $
    return ()

main :: IO ()
main = run Config{..}
    where
        routes = defaultRouter

        prime = return . app

        build = setup

setup :: Narrative App IO ()
setup =
  void $ with fusion $ do
    super setGlobalInputStyles
    super setGlobalInputFocusStyles
    embed loginForm
    child "div" Nothing $ do
      style $ do
        minHeight =: per 100
        height    =: spaces <| str auto important
        height    +: per 100
      embed bodyTop
      embed bodyBottom

mainContentContainer :: Component ()
mainContentContainer = Named {..}
  where

    tag = division

    name = mainContentName

    styles = return ()

    element = return ()

bodyTop :: Component ()
bodyTop = Atom {..}
  where

    tag = division

    styles = do
      margin    =: spaces <| str zero auto (ems $ negate footSize)

    element = do
      embed Main.header
      embed divider
      embed mainContentContainer
      embed feet

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

    styles =
      height =: ems footSize

    element = do
      embed divider
      embed Main.footer

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

    element =
      embed bottomLinks

copyrightContainer :: Component ()
copyrightContainer = Atom {..}
  where

    tag = division

    styles = do
      Flex.center
      Flex.col 100

    element =
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
      _ <- href lnk
      setText txt

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
      setText "|"

copyright :: Component ()
copyright = Atom {..}
  where

    tag = span

    styles = do
      color    =: gray
      fontSize =: px 12

    element = void $
      setText "© 2016 S. M. Hickman"

header :: Component ()
header = Atom {..}
  where

    tag = HTML.header

    styles = do
      Flex.row
      margin =: px4 0 15 6 15

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
      _ <- setText "O"
      href ""

brand :: Component ()
brand = Atom {..}
  where

    tag = anchor

    styles = do
      textDecoration =: none
      marginRight    =: px 16

    element = do
      _ <- href ""
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
        setText txt

separator :: Component ()
separator = Atom {..}
  where

    tag = span

    styles = do
        helveticaNeue (weight 200) (rgba(0,0,0,0.5)) (px 28)
        position =: relative
        top      =: px 4

    element = void $
        setText "|"

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
      embed (Main.link txt lnk)

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
      _ <- setText txt
      href lnk

hatRight :: Component ()
hatRight = Atom {..}
  where

    tag = division

    styles = do
      Flex.end
      Flex.col 10

    element =
      embed loginLink

loginLink :: Component ()
loginLink = Atom {..}
  where

    tag = anchor

    styles = do
      cursor      =: pointer
      marginRight =: px (-4)

    element = do
      embed loginGlyph
      _ <- href "loginModal"
      return ()

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

footSize :: Double
footSize = 3

mainContentName :: String
mainContentName = "main-content"

modalName :: String
modalName = "modal"

about :: HTML () -> Route ()
about = page gray gray

contact :: HTML () -> Route ()
contact = page gray gray

interesting :: HTML () -> Route ()
interesting = page black gray

privacy :: HTML () -> Route ()
privacy = page gray gray

provacative :: HTML () -> Route ()
provacative = page gray black

changeMenuHighlights :: Str -> Str -> HTML ()
changeMenuHighlights iColor pColor =
    void $ super $ do
        _ <- with "InterestingLink" $ style $ color =: iColor
        with "ProvacativeLink" $ style $ color =: pColor

page :: Str -> Str -> HTML () -> Route ()
page iColor pColor c =
    dispatch $ with mainContentName $ do
        changeMenuHighlights iColor pColor
        _ <- deleteChildren
        c
