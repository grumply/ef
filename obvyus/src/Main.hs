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
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Ef
import Ef.Type.Set
import Ef.Single

import Iron hiding (tag)

import Carbon
import Hydrogen as H
import Helium as He
import Neon as Ne


import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.Types as T
import qualified GHCJS.DOM.KeyboardEvent as K
import qualified GHCJS.DOM.UIEvent as U

import qualified Control.Monad.Trans.Class as Trans

import Obvyus
import Menu
import Listing
import Listings

import Data.Promise

import Control.Monad
import Prelude hiding (div,span)

import Unsafe.Coerce

{-
Add generic event listening for hover

Consider allowing set and reset for pairs of events like onMouseIn/onMouseOut

-}

data Tag self super result = Tag
    { tag :: String
    , name :: String
    , styles :: Narrative '[Carbon] (Narrative self super) ()
    , element :: Narrative '[Hydrogen] (Narrative self super) result
    }

named :: (Monad super, Lift IO super)
           => Tag self super result -> Narrative '[Hydrogen] (Narrative self super) result
named Tag{..} =
    fmap snd $ child tag (Just name) $ do
        style styles
        element

anon :: (Monad super, Lift IO super)
          => Tag self super result -> Narrative '[Hydrogen] (Narrative self super) result
anon Tag{..} =
    fmap snd $ child tag Nothing $ do
        style styles
        element


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

--------------------------------------------------------------------------------

footerHeight = 3

timesNewRoman weight color size =
    font "\"Times New Roman\", Serif" weight color size

helveticaNeue weight color size =
    font "\"Helvetica Neue\",Helvetica,Arial" weight color size

--------------------------------------------------------------------------------

hat = Tag {..}
  where
    
    tag = division

    styles = do
      flexibleRow
      marginBottom =: px 6

    element = do
      anon hatLeft
      anon hatRight

hatLeft = Tag {..}
  where

    tag = division

    styles = flexibleStart

    element = do
      flexibleColumn 10
      anon logo
      anon brand
      anon links

logo = Tag {..}
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

brand = Tag {..}
  where

    tag = anchor

    styles = do
      textDecoration =: none
      marginRight    =: px 16

    element = do
      href ""
      anon $ linkText "Obvy"
      anon $ separator
      anon $ linkText "us"

linkText txt = Tag {..}
  where

    tag = span

    styles = do
        helveticaNeue (weight 600) black (px 13)

    element = do
        text txt

separator = Tag {..}
  where

    tag = span

    styles = do
        helveticaNeue (weight 200) (rgba 0 0 0 0.5) (px 28)
        position =: relative
        top      =: px 4

    element = do
        text "|"

links = Tag {..}
  where

    tag = unordered

    styles = do
        listStyle =: none

    element = do
        named $ link "Interesting" "/interesting"
        named $ link "Provacative" "/provacative"

link txt lnk = Tag {..}
  where

    tag = item

    name = txt ++ "Link"

    styles = do
      display =: inline
      textDecoration =: none
      fontSize       =: px 14
      color          =: gray
      marginRight    =: px 12

    element = do
      text txt
      href lnk

hatRight = Tag {..}
  where

    tag = division

    styles = do
      flexibleEnd

    element = do
      flexibleColumn 2
      anon loginLink

loginLink = Tag {..}
  where

    tag = anchor

    styles = do
      cursor      =: pointer
      marginRight =: px (-4)

    element = do
      anon loginGlyph
      fst <$> listen E.click (const ()) listenOpts

loginGlyph = Tag {..}
  where

    tag = span

    styles = do
      top            =: px 8
      position       =: relative
      textDecoration =: none
      fontSize       =: px 20
      color          =: darkcyan

    element = do
      glyph gLogIn

--------------------------------------------------------------------------------

divider = Tag {..}
  where

    tag = division

    styles = do
      flexibleRow

    element = do
      anon spacer
      anon rule
      anon spacer

spacer = Tag {..}
  where

    tag = "division"

    styles = return ()

    element = do
      flexibleColumn 1

rule = Tag {..}
  where

    tag = hr

    styles =  do
      border  =: zero
      height  =: px 1
      margin  =: px2 5 0
      bgImage =: "linear-gradient(to right,\
                 \  rgba(0,0,0,0),\
                 \  rgba(0,0,0,0.75),\
                 \  rgba(0,0,0,0)\
                 \)"

    element = return ()

--------------------------------------------------------------------------------

shoes = Tag {..}
  where

    tag = footer

    styles = do
      flexibleRow

    element = do
      anon bottomLinkContainer
      anon copyrightContainer

bottomLinkContainer = Tag {..}
  where

    tag = division

    styles = do
      flexibleCenter

    element = do
      flexibleColumn 12
      anon bottomLinks

copyrightContainer = Tag {..}
  where

    tag = division

    styles = do
      flexibleCenter

    element = do
      flexibleColumn 12
      anon copyright

bottomLinks = Tag {..}
  where

    tag = unordered

    styles = do
      listStyle =: none
      margin    =: zero
      padding   =: zero

    element = do
      anon $ bottomLink "About" "/about"
      anon $ bottomSeparator
      anon $ bottomLink "Contact" "/contact"
      anon $ bottomSeparator
      anon $ bottomLink "Privacy" "/privacy"

bottomLink txt lnk = Tag {..}
  where

    tag = item

    styles = do
      display        =: inline
      textDecoration =: none
      color          =: gray
      fontSize       =: px 12

    element = do
      href lnk
      text txt

bottomSeparator = Tag {..}
  where

    tag = item

    styles = do
      display  =: inline
      color    =: gray
      fontSize =: px 12
      margin   =: px2 0 10

    element = return ()

copyright = Tag {..}
  where

    tag = span

    styles = do
      color    =: gray
      fontSize =: px 12

    element = do
      text "© 2016 S. M. Hickman"

--------------------------------------------------------------------------------

mainContent :: String
mainContent = "main-content"

mainContentContainer = Tag {..}
  where

    tag = division

    name = mainContent

    styles = return ()

    element = return ()

loginModal = Tag {..}
  where

    tag = division

    name = "LoginModal"

    styles = do
      flexibleContainer
      display    =: none
      position   =: fixed
      zIndex     =: one
      left       =: zero
      top        =: zero
      width      =: per 100
      height     =: per 100
      overflow   =: auto
      bgColor    =: rgb 0 0 0
      bgColor    +: rgba 0 0 0 0.4
      transition =: spaces <| str opacity (sec 0.5) linear

    element = do
        -- add backdrop click listener
        anon modal

modal = Tag {..}
  where

    tag = division

    styles = do
      flexibleRow
      top        =: per 20
      left       =: per 20
      bgColor    =: rgb 40 40 66
      height     =: per 80
      width      =: per 80
      position   =: fixed
      transition =: commas <| do
        spaces $ str transform_ (sec 0.3) easeOut
        spaces $ str visibility (sec 0.3) easeOut

    element = do
      anon modalCloseButton

modalCloseButton = Tag {..}
  where

    tag = span

    styles = do
      color      =: hex 0xaaa
      float      =: right
      fontSize   =: px 28
      fontWeight =: bold

    element = do
      text "X"
      fst <$> listen E.click (const ()) listenOpts

-- Have to be a little tricky here; we create a new signal of Esc keypresses
-- combined with close button clicks every time the login modal is opened and
-- then clean up both when it closes.
loginModalHandler :: Signal App IO ()
                  -> Signal App IO ()
                  -> Narrative '[Hydrogen] (Narrative App IO) ()
loginModalHandler loginClicks closes = void $
    behavior loginClicks $ \_ ev -> do
        (escs,unlistenEscs) <- keyUp 27 listenOpts
        (exits,escapeToken,closesToken) <- mergeSignals' escs closes
        with "LoginModal" $ style $ display =: block
        void $ behavior' exits $ \Reactor{..} _ -> do
            with "LoginModal" $ style $ display =: none
            unlistenEscs
            stop' closesToken
            stop' escapeToken
            end

--------------------------------------------------------------------------------

pageMain = Tag {..}
  where

    tag = division

    styles = do
      minHeight =: per 100
      height    =: spaces <| str auto important
      height    +: per 100
      margin    =: spaces <| str zero auto (ems $ negate footerHeight)

    element = do
      loginClicks <- anon hat
      anon divider
      modalCloseClicks <- named loginModal
      named mainContentContainer
      loginModalHandler loginClicks modalCloseClicks
      anon pushDiv
      return loginClicks

pushDiv = Tag {..}
  where

    tag = division

    styles = do
      height =: ems footerHeight

    element = return ()

pageBottom = Tag {..}
  where

    tag = division

    styles = do
      flexibleContainer
      height =: ems footerHeight

    element = do
      anon divider
      anon shoes

--------------------------------------------------------------------------------

changeMenuHighlights iColor pColor =
    super $ do
        with "InterestingLink" $ change $ color =: iColor
        with "ProvacativeLink" $ change $ color =: pColor

--------------------------------------------------------------------------------

pg :: Str
   -> Str
   -> Narrative '[Hydrogen] (Narrative App IO) ()
   -> Narrative '[Magnesium] (Narrative App IO) ()
pg i p c =
    dispatch $ with mainContent $ do
        changeMenuHighlights i p
        deleteChildren
        c

interesting = pg black gray
provacative = pg gray  black
about       = pg gray  gray
contact     = pg gray  gray
privacy     = pg gray  gray

--------------------------------------------------------------------------------

main :: IO ()
main = run Config{..}
    where
        routes = do
            path "/interesting" $ interesting $ do
                void $ change $ height =: px 1200

            path "/about" $ about $ do
                return ()

            path "/contact" $ contact $ do
                return ()

            path "/privacy" $ privacy $ do
                return ()

            -- default
            provacative $ do
                void $ change $ clear height

        prime base = return (app base)

        build = setup

setup = do
  void $ with fusion $ do
    style $ do
      height =: per 100
      flexibleContainer
    anon pageMain
    anon pageBottom
  void addFusion
