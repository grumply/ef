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
  super setGlobalSubmitHoverStyles
  super setGlobalActiveStyles
  _ <- embed signupForm
  _ <- embed loginForm
  return ()

--------------------------------------------------------------------------------
-- Shared form styles

setGlobalInputStyles = do
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
   styleGlobal (string "input[type=submit]") $ do
        backgroundColor =: hsl(180,100,27)

setGlobalInputFocusStyles =
  globalFocusStyle (string "input[type=text]:focus,input[type=password]:focus") $ do
    boxShadow =: commas <| do
      restr $ spaces <| str zero zero (px 5) (rgba(81,203,238,1))
      restr $ spaces <| str inset (px 1) (px 1) (px 2) (px (-1))
    padding   =: px4 3 0 3 8
    margin    =: px4 5 1 3 0
    border    =: spaces <| str (px 1) solid (rgba(81,203,238,1))

setGlobalSubmitHoverStyles =
  globalHoverStyle (string "input[type=submit]:hover") $ do
        backgroundColor =: hsl(180,100,20)
        glow darkcyan

setGlobalActiveStyles =
  globalActiveStyle (string "input[type=submit]:active") $ do
        backgroundColor =: hsl(180,100,13)

glow color = do
  boxShadow =: commas <| do
    restr $ spaces <| str zero zero (px 8) color
    restr $ spaces <| str inset (px 1) (px 1) (px 2) (px (-1))
  border    =: spaces <| str (px 1) solid color

unglow = do
  clearStyle boxShadow
  clearStyle border

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

    element = do
      _ <- embed lnk
      _ <- embed but
      return ()

-- button
b name txt = Named {..}
  where

    tag = input

    styles = do
      col 40
      border          =: spaces <| str (px 1) solid transparent
      textAlign       =: CSS.center
      marginTop       =: px 12
      timesNewRoman CSS.bold white (px 18)

    element = do
      type_ submit
      value_ txt

-- link
l txt lnk = Atom {..}
  where

    tag = anchor

    styles = do
      col 60
      marginTop =: px 12
      textAlign =: left
      paddingTop =: px 6
      fontSize =: px 18

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
      return ()

signupValidater = do
  (signupSubmitClicks,_) <- with "signupSubmit" (listen E.click id interceptOpts)
  behavior' signupSubmitClicks $ \_ _ -> do
    username <- with "signupUsername" $ do
      Just un <- getInputValue
      when (length un < 4) $ void $ style $ glow orange
      when (null un) $ void $ style $ glow red
      return un
    let hasUsername = length username >= 4
    password <- with "signupPassword" $ do
      Just p <- getInputValue
      when (length p < 8) $ void $ style $ glow orange
      when (null p) $ void $ style $ glow red
      return p
    let hasPassword = length password >= 8
    confirmPassword <- with "signupConfirmPassword" $ do
      Just cp <- getInputValue
      when (cp /= password || null cp) $ void $ style $ glow red
      return cp
    let hasConfirmPassword = confirmPassword == password
    email <- with "signupEmail" $ do
      Just e <- getInputValue
      when (null e) $ void $ style $ glow red
      return e
    let hasEmail = not (null email)
    confirmEmail <- with "signupConfirmEmail" $ do
      Just ce <- getInputValue
      when (ce /= email || null ce) $ void $ style $ glow red
      return ce
    let hasConfirmEmail = confirmEmail == email
    when (hasUsername && hasPassword && hasConfirmPassword && hasConfirmEmail) $ do
      -- check username, email, and password here
      -- get loginKey for cookie storage
      -- set logged in
      -- maybe redirect to new user page
      setLocation "#close"

  (signupUsernameInput,_) <- with "signupUsername" (listen E.input id listenOpts)
  behavior' signupUsernameInput $ \_ _ -> do
    with "signupUsername" $ do
      Just un <- getInputValue
      when (length un >= 4) $ void $ style unglow

  (signupPasswordInput,_) <- with "signupPassword" (listen E.input id listenOpts)
  behavior' signupPasswordInput $ \_ _ -> do
    with "signupPassword" $ do
      Just p <- getInputValue
      when (length p >= 8) $ void $ style unglow
  (signupConfirmPasswordInput,_) <- with "signupConfirmPassword" (listen E.input id listenOpts)
  behavior' signupConfirmPasswordInput $ \_ _ -> do
    Just p <- with "signupPassword" getInputValue
    with "signupConfirmPassword" $ do
      Just pc <- getInputValue
      when (pc /= p) $ void $ style $ glow red
      when (pc == p) $ void $ style unglow

  (signupEmailInput,_) <- with "signupEmail" (listen E.input id listenOpts)
  behavior' signupEmailInput $ \_ _ -> do
    with "signupEmail" $ do
      void $ style unglow
  (signupConfirmEmailInput,_) <- with "signupConfirmEmail" (listen E.input id listenOpts)
  behavior' signupConfirmEmailInput $ \_ _ -> do
    Just e <- with "signupEmail" getInputValue
    with "signupConfirmEmail" $ do
      Just ec <- getInputValue
      when (ec /= e) $ void $ style $ glow red
      when (ec == e) $ void $ style unglow

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
      super loginValidater
      return ()

loginValidater = do
  (loginSubmitClicks,_) <- with "loginSubmit" (listen E.click id interceptOpts)
  behavior' loginSubmitClicks $ \_ _ -> do
    hasUsername <- with "loginUsername" $ do
      Just un <- getInputValue
      when (length un < 4) $ void $ style $ glow orange
      when (null un) $ void $ style $ glow red
      return (length un >= 4)
    hasPassword <- with "loginPassword" $ do
      Just p <- getInputValue
      when (length p < 8) $ void $ style $ glow orange
      when (null p) $ void $ style $ glow red
      return (length p >= 8)
    when (hasUsername && hasPassword) $ do
      -- check username and password here
      -- get loginKey for cookie storage
      -- set logged in
      setLocation "#close"

  (loginUsernameInput,_) <- with "loginUsername" (listen E.input id listenOpts)
  behavior' loginUsernameInput $ \_ _ -> do
    with "loginUsername" $ do
      Just un <- getInputValue
      when (length un >= 4) $ void $ style unglow

  (loginPasswordInput,_) <- with "loginPassword" (listen E.input id listenOpts)
  behavior' loginPasswordInput $ \_ _ -> do
    with "loginPassword" $ do
      Just p <- getInputValue
      when (length p >= 8) $ void $ style unglow
