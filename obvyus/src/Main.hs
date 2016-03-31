{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language NoMonomorphismRestriction #-}
{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}
{-# language CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Ef
import Ef.Single
import Data.Promise

import Iron

import Carbon as CSS hiding (text)
import Hydrogen as HTML
import Helium as Str
import Neon as Glyph


import qualified GHCJS.DOM.Element as E

import Obvyus
import Menu
import Listings

import Data.String
import Control.Monad
import Prelude hiding (div,span,all)
import qualified Prelude


#ifdef HLINT
{-# ANN module "HLint: ignore Redundant do" :: String #-}
#endif

--------------------------------------------------------------------------------

app base = listings *:* listings *:* obvyus *:* mkMenu *:* base

type App = '[ Listings ProvocativeListings
            , Listings InterestingListings
            , Obvyus
            , Menu
            , Oxygen
            , Silicon
            , SingleKnot
            ]

type CSS = Narrative '[Carbon] (Narrative App IO)
type HTML = Narrative '[Hydrogen] (Narrative App IO)
type Route = Narrative '[Magnesium] (Narrative App IO)

--------------------------------------------------------------------------------

footSize :: Double
footSize = 3

timesNewRoman :: Str -> Str -> Str -> CSS ()
timesNewRoman weight_ color_ size_ =
    font "\"Times New Roman\", Serif"
         weight_
         color_
         size_

helveticaNeue :: Str -> Str -> Str -> CSS ()
helveticaNeue weight_ color_ size_ =
    font "\"Helvetica Neue\",Helvetica,Arial"
         weight_
         color_
         size_

--------------------------------------------------------------------------------

hat = Component {..}
  where

    tag = header

    styles = do
      flexibleRow
      marginBottom =: px 6

    element = do
      unnamed hatLeft
      unnamed hatRight

hatLeft = Component {..}
  where

    tag = division

    styles = flexibleStart

    element = do
      flexibleColumn 10
      unnamed logo
      unnamed brand
      unnamed links

logo = Component {..}
  where

    tag = anchor

    styles = do
      textDecoration =: none
      bgColor        =: darkcyan
      padding        =: px2 2 5
      marginRight    =: px 10
      timesNewRoman bold white (px 15)

    element = do
      text "O"
      href ""

brand = Component {..}
  where

    tag = anchor

    styles = do
      textDecoration =: none
      marginRight    =: px 16

    element = do
      href ""
      unnamed $ linkText "Obvy"
      unnamed separator
      unnamed $ linkText "us"

linkText txt = Component {..}
  where

    tag = span

    styles =
        helveticaNeue (weight 600) black (px 13)

    element =
        text txt

separator = Component {..}
  where

    tag = span

    styles = do
        helveticaNeue (weight 200) (rgba 0 0 0 0.5) (px 28)
        position =: relative
        top      =: px 4

    element =
        text "|"

links = Component {..}
  where

    tag = unordered

    styles = do
      display   =: inline
      listStyle =: none

    element = do
      unnamed $ linkBox "Interesting" "/interesting"
      unnamed $ linkBox "Provacative" "/provacative"

linkBox txt lnk = Component {..}
  where

    tag = item

    styles =
      display =: inline

    element =
      named (link txt lnk)


link txt lnk = Component {..}
  where

    tag = anchor

    name = txt ++ "Link"

    styles = do
      cursor         =: pointer
      textDecoration =: none
      fontSize       =: px 14
      color          =: gray
      marginRight    =: px 12

    element = do
      text txt
      href lnk

hatRight = Component {..}
  where

    tag = division

    styles =
      flexibleEnd

    element = do
      flexibleColumn 2
      unnamed loginLink

loginLink = Component {..}
  where

    tag = anchor

    styles = do
      cursor      =: pointer
      marginRight =: px (-4)

    element = do
      unnamed loginGlyph
      fst <$> listen E.click (const ()) listenOpts

loginGlyph = Component {..}
  where

    tag = span

    styles = do
      top            =: px 8
      position       =: relative
      textDecoration =: none
      fontSize       =: px 20
      color          =: darkcyan

    element =
      glyph gLogIn

--------------------------------------------------------------------------------

divider = Component {..}
  where

    tag = division

    styles =
      flexibleRow

    element = do
      unnamed spacer
      unnamed rule
      unnamed spacer

spacer = Component {..}
  where

    tag = division

    styles = return ()

    element =
      flexibleColumn 1

rule = Component {..}
  where

    tag = hr

    styles =  do
      border  =: zero
      height  =: px 1
      margin  =: px2 5 0
      bgImage =: "linear-gradient(to right,rgba(0,0,0,0),rgba(0,0,0,0.75),rgba(0,0,0,0))"

    element =
      flexibleColumn 10

--------------------------------------------------------------------------------

shoes = Component {..}
  where

    tag = footer

    styles =
      flexibleRow

    element = do
      unnamed bottomLinkContainer
      unnamed copyrightContainer

bottomLinkContainer = Component {..}
  where

    tag = division

    styles =
      flexibleCenter

    element = do
      flexibleColumn 12
      unnamed bottomLinks

copyrightContainer = Component {..}
  where

    tag = division

    styles =
      flexibleCenter

    element = do
      flexibleColumn 12
      unnamed copyright

bottomLinks = Component {..}
  where

    tag = unordered

    styles = do
      listStyle =: none
      margin    =: zero
      padding   =: zero

    element = do
      unnamed $ bottomLinkBox "About" "/about"
      unnamed bottomSeparator
      unnamed $ bottomLinkBox "Contact" "/contact"
      unnamed bottomSeparator
      unnamed $ bottomLinkBox "Privacy" "/privacy"

bottomLinkBox txt lnk = Component {..}
  where

    tag = item

    styles =
      display =: inline

    element =
      unnamed (bottomLink txt lnk)

bottomLink txt lnk = Component {..}
  where

    tag = anchor

    styles = do
      textDecoration =: none
      color          =: gray
      fontSize       =: px 12

    element = do
      href lnk
      text txt

bottomSeparator = Component {..}
  where

    tag = item

    styles = do
      display  =: inline
      color    =: gray
      fontSize =: px 12
      margin   =: px2 0 10

    element =
      text "|"

copyright = Component {..}
  where

    tag = span

    styles = do
      color    =: gray
      fontSize =: px 12

    element =
      text "© 2016 S. M. Hickman"

--------------------------------------------------------------------------------

mainContent :: String
mainContent = "main-content"

mainContentContainer = Component {..}
  where

    tag = division

    name = mainContent

    styles = return ()

    element = return ()

--------------------------------------------------------------------------------

loginModal = Component {..}
  where

    tag = division

    name = "LoginModal"

    styles = do
      flexibleContainer
      visibility =: hidden
      position   =: fixed
      zIndex     =: one
      margin     =: none
      left       =: zero
      top        =: zero
      width      =: per 100
      height     =: per 100
      overflow   =: auto
      bgColor    =: rgb 0 0 0
      bgColor    +: rgba 0 0 0 0.4
      transition =: commas <| do
          restring $ spaces <| str width (sec 0.3) easeIn
          restring $ spaces <| str height (sec 0.3) easeIn

    element =
        -- add backdrop click listener
        unnamed modal

modalBox = Component {..}
  where

    tag = division

    styles = do
      flexibleRow
      flexibleCenter

    element =
      unnamed modal

modal = Component {..}
  where

    tag = division

    styles = do
      bgColor    =: white

    element = do
      responsive 11 11 8 8
      unnamed modalCloseButton

modalCloseButton = Component {..}
  where

    tag = span

    styles = do
      color      =: hex 0xaaa
      float      =: right
      fontSize   =: px 20

    element = do
      glyph gRemoveSign
      fst <$> listen E.click (const ()) listenOpts

-- Have to be a little tricky here; we create a new signal of Esc keypresses
-- combined with close button clicks every time the login modal is opened and
-- then clean up both when it closes.
loginModalHandler :: Signal App IO ()
                  -> Signal App IO ()
                  -> Narrative '[Hydrogen] (Narrative App IO) ()
loginModalHandler loginModalOpens closes = void $
    behavior loginModalOpens $ \_ _ -> do
        (escs,unlistenEscs) <- keyUp 27 listenOpts
        (exits,escapeToken,closesToken) <- mergeSignals' escs closes
        with "LoginModal" $ change $
            visibility =: visible
        void $ behavior' exits $ \r _ -> do
            with "LoginModal" $ change $
                visibility =: hidden
            unlistenEscs
            stop' closesToken
            stop' escapeToken
            Iron.end r

--------------------------------------------------------------------------------

bodyTop = Component {..}
  where

    tag = division

    styles = do
      minHeight =: per 100
      height    =: spaces <| str auto important
      height    +: per 100
      margin    =: spaces <| str zero auto (ems $ negate footSize)

    element = do
      loginClicks <- unnamed hat
      unnamed divider
      modalCloseClicks <- named loginModal
      named mainContentContainer
      loginModalHandler loginClicks modalCloseClicks
      unnamed feet
      return loginClicks

feet = Component {..}
  where

    tag = division

    styles =
      height =: ems footSize

    element = return ()

bodyBottom = Component {..}
  where

    tag = division

    styles = do
      flexibleContainer
      height =: ems footSize

    element = do
      unnamed divider
      unnamed shoes

--------------------------------------------------------------------------------

changeMenuHighlights iColor pColor =
    super $ do
        with "InterestingLink" $ change $ color =: iColor
        with "ProvacativeLink" $ change $ color =: pColor

--------------------------------------------------------------------------------

pg :: Str -> Str -> HTML () -> Route ()
pg iColor pColor c =
    dispatch $ with mainContent $ do
        changeMenuHighlights iColor pColor
        deleteChildren
        c

interesting :: HTML () -> Route ()
interesting = pg black gray

provacative :: HTML () -> Route ()
provacative = pg gray black

about :: HTML () -> Route ()
about = pg gray gray

contact :: HTML () -> Route ()
contact = pg gray gray

privacy :: HTML () -> Route ()
privacy = pg gray gray

--------------------------------------------------------------------------------

main :: IO ()
main = run Config{..}
    where
        routes = do
            path "/interesting" $ interesting $
                void $ change $ height =: px 1200

            path "/about" $ about $
                return ()

            path "/contact" $ contact $
                return ()

            path "/privacy" $ privacy $
                return ()

            -- default
            provacative $
                void $ change $ clear height

        prime base = return (app base)

        build = setup

setup = do
  void $ with fusion $ do
    style $ do
      height =: per 100
      flexibleContainer
    unnamed bodyTop
    unnamed bodyBottom
  void addFusion
