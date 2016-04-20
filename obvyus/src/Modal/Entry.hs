{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language NoMonomorphismRestriction #-}
module Modal.Entry where

import Ef

import Carbon as CSS
import Helium
import Hydrogen
import Iron

import Flex

import Modal
import Utility

entryModal = modal "entry" $ do
  _ <- embed entryModalHeader
  _ <- embed divider
  _ <- embed entryForms
  return ()

entryModalHeader = Atom {..}
  where

    tag = h2

    styles = do
      marginTop     =: px 15
      textAlign     =: CSS.center
      textTransform =: uppercase
      timesNewRoman (weight 600) slategray (px 22)

    element =
      setText "Sign up or Log in"

entryForms = Atom {..}
  where

    tag = division

    styles = do
      Flex.row
      Flex.center

    element = do
      super setGlobalInputStyles
      super setGlobalInputFocusStyles
      _ <- embed signupForm
      _ <- embed loginForm
      return ()


--------------------------------------------------------------------------------
-- Shared form styles and content

formStyles = do
  marginTop      =: px 20
  marginRight    =: px 20
  background     =: slategray
  border         =: none
  color          =: white
  padding        =: px2 12 24
  borderRadius   =: px 13
  fontSize       =: px 18
  fontFamily     =: "Georgia, Serif"
  textDecoration =: uppercase

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

usernameInput nm = Atom {..}
  where

    tag = input

    styles = col 80

    element = do
      name_ nm
      type_ text
      placeholder_ "username"
      required_

passwordInput nm = Atom {..}
  where

    tag = input

    styles = col 80

    element = do
      name_ nm
      type_ password
      placeholder_ password
      required_

--------------------------------------------------------------------------------
-- Signup Form

signupForm = Atom {..}
  where

    tag = form

    styles = do
      padding    =: px 20
--      visibility =: hidden

    element = do
      flexible col 80 80 50 50
      responsive borderRight
          none
          none
          (spaces <| str (px 1) solid (hex 0xe0e0e0))
          (spaces <| str (px 1) solid (hex 0xe0e0e0))
      embed signupHeader
      embed signupFormInputFields
      return ()

signupHeader = Atom {..}
  where

    tag = h2

    styles = return ()

    element =
      setText "Sign up"

signupFormInputFields = Atom {..}
  where

    tag = division

    styles = do
      Flex.row
      Flex.center

    element = do
      _ <- embed $ usernameInput "signupUsername"
      _ <- embed emailInput
      _ <- embed $ passwordInput "signupPassword"
      _ <- embed passwordConfirmInput
      _ <- embed signupFormSubmitButton
      return ()

emailInput = Atom {..}
  where

    tag = input

    styles = col 80

    element = do
      name_ "email"
      type_ text
      placeholder_ "email"
      required_

passwordConfirmInput = Atom {..}
  where

    tag = input

    styles = col 80

    element = do
      type_ password
      placeholder_ "confirm password"
      required_

signupFormSubmitButton = Atom {..}
  where

    tag = input

    styles = do
      formStyles

    element = do
      type_ submit
      value_ "Sign up"

--------------------------------------------------------------------------------
-- Login Form

loginForm = Atom {..}
  where

    tag = form

    styles =
      padding =: px 20

    element = do
      flexible col 80 80 50 50
      embed loginHeader
      embed loginFormInputFields
      embed loginFormSubmitButton

loginHeader = Atom {..}
  where

    tag = h2

    styles = return ()

    element =
      setText "Log in"

loginFormInputFields = Atom {..}
  where

    tag = division

    styles = do
      Flex.row
      Flex.center

    element = do
      _ <- embed $ usernameInput "loginUsername"
      _ <- embed $ passwordInput "loginPassword"
      return ()

loginFormSubmitButton = Atom {..}
  where

    tag = input

    styles = formStyles

    element = do
      type_ submit
      value_ "Log in"
