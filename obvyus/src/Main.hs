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

import Control.Monad

import qualified GHCJS.DOM.Element as E

import Prelude hiding (span)

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

loginModal = Named {..}
  where

    tag = division

    name = "loginModal"

    styles = do
      Flex.row
      position      =: fixed
      top           =: zero
      right         =: zero
      bottom        =: zero
      left          =: zero
      zIndex        =: int 9998
      background    =: rgba(0,0,0,0.8)
      transition    =: spaces <| str opacity (ms 400) easeIn

    element = do
      super $ stylePage (individual "loginModal") $ do
        opacity =: zero
        pointerEvents =: none
      super $ stylePage (target (individual "loginModal")) $ do
        opacity =: one
        pointerEvents =: auto
      embed loginModalContent

loginModalClose = Named {..}
  where

    tag = anchor

    name = "loginModalClose"

    styles = do
      fontFamily    =: "Arial, Helvetica, sans-serif"
      color          =: hex 0xFFFFFF
      lineHeight     =: px 25
      position       =: absolute
      right          =: px (-12)
      textAlign      =: CSS.center
      top            =: px (-10)
      width          =: px 24
      textDecoration =: none
      fontWeight     =: bold
      borderRadius   =: px 12
      zIndex         =: int 9999
      boxShadow      =: str (px 1) (px 1) (px 3) (hex 0x000)

    element = do
      super $ stylePage (individual "loginModalClose") $
        background =: hex 0x606060
      super $ stylePage (hover (individual "loginModalClose")) $
        background =: hex 0x00d9ff
      _ <- href "close"
      text "X"

loginModalContent = Named {..}
  where

    tag = division

    name = "loginModalContent"

    styles = do
      position     =: relative
      margin       =: spaces <| str (per 10) auto
      padding      =: px 20
      borderRadius =: px 10
      background   =: linearGradient <| str (string "aliceblue") azure

    element = do
      flexible col 85 85 50 50
      _ <- embed loginModalClose
      _ <- embed loginModalHeader
      _ <- embed divider
      _ <- embed modalFormsContainer
      return ()


loginModalHeader = Atom {..}
  where

    tag = h2

    styles = do
      margin =: px2 10 10
      textAlign =: CSS.center
      textTransform =: uppercase
      timesNewRoman (weight 600) slategray (px 22)

    element =
      text "Log in or Sign up"

modalFormsContainer = Atom {..}
  where

    tag = form

    styles = do
      Flex.row
      Flex.center

    element = do
      super setGlobalInputStyles
      super setGlobalInputFocusStyles
      _ <- embed loginForm
      _ <- embed signupForm
      return ()

loginForm = Atom {..}
  where

    tag = division

    styles = return ()

    element = do
      flexible col 80 80 40 40
      responsive (borderRight =:)
        none
        none
        (spaces <| str (px 5) solid black)
        (spaces <| str (px 5) solid black)
      responsive (borderRight =:)
        none
        none
        (spaces <| str (px 1) solid black)
        (spaces <| str (px 1) solid black)
      embed loginFormInputFields
      return ()

loginFormInputFields = Atom {..}
  where

    tag = division

    styles = do
      Flex.row
      Flex.center

    element = do
      _ <- embed usernameInput
      _ <- embed emailInput
      _ <- embed passwordInput
      _ <- embed passwordConfirmInput
      return ()

setGlobalInputStyles =
   styleGlobal (string "input[type=text],input[type=password]") $ do
     boxShadow    =: spaces <| str inset (px 1) (px 1) (px 2) (px (-1))
     border       =: spaces <| str (px 1) solid (hex 0xDDDDDD)
     transition   =: spaces <| str CSS.all (sec 0.3) easeInOut
     outline      =: none
     padding      =: px4 3 0 3 8
     margin       =: px4 5 1 3 0
     borderRadius =: px 5
     height       =: px 34
     lineHeight   =: px 20
     helveticaNeue (weight 400) slategray (px 16)

setGlobalInputFocusStyles =
  styleGlobal (string "input[type=text]:focus,input[type=password]:focus") $ do
    boxShadow =: commas <| do
      restr $ spaces <| str zero zero (px 5) (rgba(81,203,238,1))
      restr $ spaces <| str inset (px 1) (px 1) (px 2) (px (-1))
    padding   =: px4 3 0 3 8
    margin    =: px4 5 1 3 0
    border    =: spaces <| str (px 1) solid (rgba(81,203,238,1))

usernameInput = Atom {..}
  where

    tag = input

    styles = col 80

    element = do
      setAttr "name" "username"
      setAttr "type" "text"
      setAttr "placeholder" "username"
      setAttr "required" ""

emailInput = Atom {..}
  where

    tag = input

    styles = col 80

    element = do
      setAttr "name" "email"
      setAttr "type" "text"
      setAttr "placeholder" "email"
      setAttr "required" ""

passwordInput = Atom {..}
  where

    tag = division

    styles = col 80

    element = do
      setAttr "name" "password"
      setAttr "type" "password"
      setAttr "placeholder" "password"
      setAttr "required" ""

passwordConfirmInput = Atom {..}
  where

    tag = input

    styles = col 80

    element = do
      setAttr "type" "password"
      setAttr "placeholder" "confirm password"
      setAttr "required" ""

signupForm = Atom {..}
  where

    tag = division

    styles = return ()

    element = do
      flexible col 80 80 40 40

setup :: Narrative App IO ()
setup =
  void $ with fusion $ do
    embed loginModal
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

divider :: Component ()
divider = Atom {..}
  where

    tag = division

    styles =
      Flex.row

    element = do
      embed spacer
      embed rule
      embed spacer

spacer :: Component ()
spacer = Atom {..}
  where

    tag = division

    styles =
      Flex.col 10

    element =
      return ()

rule :: Component ()
rule = Atom {..}
  where

    tag = hr

    styles =  do
      Flex.col 80
      border  =: zero
      height  =: px 1
      margin  =: px2 5 0
      bgImage =: "linear-gradient(to right,rgba(0,0,0,0),rgba(0,0,0,0.75),rgba(0,0,0,0))"

    element = return ()

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
      _ <- text "O"
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
      _ <- text txt
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

timesNewRoman weight_ color_ size_ =
    font "\"Times New Roman\", Serif"
         weight_
         color_
         size_

helveticaNeue weight_ color_ size_ =
    font "\"Helvetica Neue\",Helvetica,Arial"
         weight_
         color_
         size_

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
