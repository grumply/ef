{-# language OverloadedStrings #-}
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

import Control.Monad

entryForms = do
  super setGlobalInputStyles
  super setGlobalInputFocusStyles
  _ <- embed signupForm
  _ <- embed loginForm
  return ()

--------------------------------------------------------------------------------
-- Shared form styles

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

-- header
h txt = Atom {..}
  where

    tag = h2

    styles = do
      col 100
      textAlign     =: CSS.center
      textTransform =: uppercase
      timesNewRoman (weight 600) slategray (px 22)

    element =
      setText txt

-- input
i name typ plchldr = Named {..}
  where

    tag = input

    styles = col 80

    element = do
      name_ name
      type_ typ
      placeholder_ plchldr
      required_

s = Atom {..}
  where

    tag = division

    styles = return ()

    element = do
      flexible col 10 10 25 25

formBottom lnk but = Atom {..}
  where

    tag = division

    styles = do
      Flex.row
      col 80
      margin =: spaces <| str (px 10) auto

    element = do
      _ <- embed lnk
      _ <- embed but
      return ()

-- button
b name txt = Named {..}
  where

    tag = input

    styles = do
      col 20

    element = do
      type_ submit
      value_ txt

-- link
l txt lnk = Atom {..}
  where

    tag = anchor

    styles = do
      col 70
      textAlign =: left

    element = do
      href lnk
      setText txt

--------------------------------------------------------------------------------
-- Signup Form

signupForm = modal "signup" $ do
  _ <- embed $ h "Sign up"
  _ <- embed divider
  _ <- embed Atom {..}
  return ()
  where

    tag = form

    styles = do
      Flex.row
      Flex.center
      marginBottom =: px 15

    element = do
      _ <- embed $ i "signupUsername" text "username"
      _ <- embed $ i "signupEmail" text "email"
      _ <- embed $ i "signupConfirmEmail" text "confirm email"
      _ <- embed $ i "signupPassword" password "password"
      _ <- embed $ i "signupConfirmPassword" password "confirm password"
      _ <- embed $ formBottom (l "Log in" "loginModal") (b "signupSubmit" "Sign up")
      super signupValidater
      super showInvalidOnUnavailableUsername
      super showInvalidOnShortPassword
      super showInvalidOnPasswordMismatch
      super showInvalidOnEmailMismatch
      return ()

--------------------------------------------------------------------------------
-- Login form

loginForm = modal "login" $ do
  _ <- embed $ h "Log in"
  _ <- embed divider
  _ <- embed Atom {..}
  return ()
  where

    tag = form

    styles = do
      Flex.row
      Flex.center
      marginBottom =: px 15

    element = do
      _ <- embed $ i "loginUsername" text "username"
      _ <- embed $ i "loginPassword" password "password"
      _ <- embed $ formBottom (l "Sign up" "signupModal") (b "loginSubmit" "Log in")
      return ()

--------------------------------------------------------------------------------
-- Signup form validation/interaction

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
