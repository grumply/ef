{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language NoMonomorphismRestriction #-}
{-# language TypeOperators #-}
module Modal.Entry where

import Ef

import Carbon as CSS
import Helium
import Hydrogen
import Iron

import Flex

import Modal
import Utility

import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.HTMLInputElement as IE

import Control.Monad

--------------------------------------------------------------------------------
-- Shared form styles

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

--------------------------------------------------------------------------------
-- Form inputs

usernameInput nm name = Named {..}
  where

    tag = input

    styles = col 80

    element = do
      name_ nm
      type_ text
      placeholder_ "username"
      required_

passwordInput nm name = Named {..}
  where

    tag = input

    styles = col 80

    element = do
      name_ nm
      type_ password
      placeholder_ password
      required_

passwordConfirmInput name = Named {..}
  where

    tag = input

    styles = col 80

    element = do
      type_ password
      placeholder_ "confirm password"
      required_

emailInput name = Named {..}
  where

    tag = input

    styles = col 80

    element = do
      name_ "email"
      type_ text
      placeholder_ "email"
      required_

emailConfirmInput name = Named {..}
  where

    tag = input

    styles = col 80

    element = do
      name_ "emailConfirm"
      type_ text
      placeholder_ "confirm email"
      required_

--------------------------------------------------------------------------------
-- Signup Form

signupFormContainer = Atom {..}
  where

    tag = division

    styles = do
      Flex.row
      Flex.center

    element = do
      _ <- embed signupForm
      return ()

signupFormModalHeader = Atom {..}
  where

    tag = h2

    styles = do
      marginTop     =: px 15
      textAlign     =: CSS.center
      textTransform =: uppercase
      timesNewRoman (weight 600) slategray (px 22)

    element =
      setText "Sign up"

signupForm = Atom {..}
  where

    tag = form

    styles = do
      padding    =: px 20
      margin     =: spaces <| str none auto auto auto

    element = do
      flexible col 80 80 50 50
      responsive borderRight
          none
          none
          (spaces <| str (px 1) solid (hex 0xe0e0e0))
          (spaces <| str (px 1) solid (hex 0xe0e0e0))
      embed signupHeader
      embed signupFormInputFields
      super signupValidater
      super showInvalidOnUnavailableUsername
      super showInvalidOnShortPassword
      super showInvalidOnPasswordMismatch
      super showInvalidOnEmailMismatch
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
      _ <- embed $ usernameInput "signupUsername" "signupUsername"
      _ <- embed $ emailInput "signupEmail"
      _ <- embed $ emailConfirmInput "signupConfirmEmail"
      _ <- embed $ passwordInput "signupPassword" "signupPassword"
      _ <- embed $ passwordConfirmInput "signupConfirmPassword"
      _ <- embed signupFormSubmitButton
      return ()

signupFormSubmitButton = Named {..}
  where

    name = "signupSubmit"

    tag = input

    styles = do
      formStyles

    element = do
      setDisabled
      type_ submit
      value_ "Sign up"

signupValidater :: (Web <: self) => Narrative self IO ()
signupValidater = do
  (passwordInput,_)     <- with "signupPassword"        (listen E.input id listenOpts)
  (confirmPassInput,_)  <- with "signupConfirmPassword" (listen E.input id listenOpts)
  (emailInput,_)        <- with "signupEmail"           (listen E.input id listenOpts)
  (confirmEmailInput,_) <- with "signupConfirmEmail"    (listen E.input id listenOpts)
  (usernameInput,_)     <- with "signupUsername"        (listen E.input id listenOpts)
  (pass,_,_)      <- mergeSignals' passwordInput confirmPassInput
  (email,_,_)     <- mergeSignals' emailInput    confirmEmailInput
  (passemail,_,_) <- mergeSignals' pass          email
  (changes,_,_)   <- mergeSignals' passemail     usernameInput
  behavior' changes $ \_ _ -> do
    Just pass      <- with "signupPassword"        getInputValue
    let passl = length pass
    Just confPass  <- with "signupConfirmPassword" getInputValue
    let confPassl = length confPass
    Just email     <- with "signupEmail"           getInputValue
    let emaill = length email
    Just confEmail <- with "signupConfirmEmail"    getInputValue
    let confEmaill = length confEmail
    Just user      <- with "signupUsername"        getInputValue
    validUsername <- checkUsername user
    with "signupSubmit" $
      if (passl >= 8 && pass == confPass && not (null email) && email == confEmail && validUsername)
        then setEnabled
        else setDisabled
    when (passl < 8 && passl > 0) $
      void $ with "signupPassword" $ style $ backgroundColor =: red
    when (passl >= 8 && confPassl > 0 && pass /= confPass) $
      void $ with "signupConfirmPassword" $ style $ backgroundColor =: red
    when (emaill > 0 && confEmaill > 0 && email /= confEmail) $
      void $ with "signupConfirmEmail" $ style $ backgroundColor =: red
  return ()

checkUsername un
  | not (null un) = return True
  | otherwise = return False

-- disableFormSubmissionOnShortPassword =
--   with "signupPassword" $ void $ do
--     (updates,_) <- listen E.input id listenOpts
--     behavior updates $ \_ _ -> void $ do
--         Just pass <- with "signupPassword" getInputValue
--         let lpass = length pass
--             short = lpass < 8
--         with "signupSubmit" $
--           if short
--             then setDisabled
--             else setEnabled

showInvalidOnUnavailableUsername = return ()

showInvalidOnShortPassword =
  with "signupPassword" $ void $ do
    (updates,_) <- listen E.input id listenOpts
    behavior updates $ \_ _ -> void $
      with "signupPassword" $ do
        Just pass <- getInputValue
        let lpass = length pass
            short = lpass > 0 && lpass < 8
            c = if short then red else none
        style $ backgroundColor =: c

showInvalidOnPasswordMismatch = return ()

showInvalidOnEmailMismatch = return ()

--------------------------------------------------------------------------------
-- Login Form

loginForm = modal "login" $ do
  embed loginFormHeader
  embed loginFormContainer

loginFormHeader = Atom {..}
  where

    tag = h2

    styles = do
      marginTop     =: px 15
      textAlign     =: CSS.center
      textTransform =: uppercase
      timesNewRoman (weight 600) slategray (px 22)

    element =
      setText "Log in"

loginFormContainer = Atom {..}
  where

    tag = division

    styles = do
      Flex.row
      Flex.center

    element = do
      _ <- embed loginFormContent
      return ()

loginFormContent = Atom {..}
  where

    tag = form

    styles = do
      marginTop =: none
      padding =: px 20

    element = do
      flexible col 80 80 50 50
      embed loginHeader
      embed loginFormInputFields
      (submits,_) <- listen E.submit id interceptOpts
      behavior submits $ \_ _ -> do
        Just tc <- with "loginUsername" getInputValue
        Just p <- with "loginPassword" getInputValue
        lift $ print $ "login form submitted: (user,pass): " ++ show (tc,p)

loginFormInputFields = Atom {..}
  where

    tag = division

    styles = do
      Flex.row

    element = do
      _ <- embed $ usernameInput "loginUsername" "loginUsername"
      _ <- embed $ passwordInput "loginPassword" "loginPassword"
      _ <- embed loginFormSubmitButton
      return ()

loginFormSubmitButton = Atom {..}
  where

    tag = input

    styles = formStyles

    element = do
      type_ submit
      value_ "Log in"

signupLink = Atom {..}
  where

    tag = anchor

    styles = return ()

    element = do
      href "/signup"
      text "Sign up"
