{-# language RecordWildCards #-}
{-# language DataKinds #-}
module Component.Login where

import Ef

import App

import Carbon as CSS hiding (text)
import Hydrogen as HTML
import Helium as Str
import Neon as Glyph
import Oxygen
import Magnesium
import Silicon
import Iron

import Flex

import Components
import Component.Util
import Component.Modal

import qualified GHCJS.DOM.Element as E

import Control.Monad

import Prelude hiding (span)

loginContainer :: Component (Signal App IO ())
loginContainer = Atom {..}
  where

    tag = division

    styles =
      return ()

    element = do
      embed loginSpacer
      modalCloseButtonClicks <- embed loginContent
      embed loginSpacer
      modalOutsideClicks <- fst <$> listen E.click (const ()) interceptOpts
      (sig,_,_) <- mergeSignals modalCloseButtonClicks modalOutsideClicks
      return sig
      -- embed modalCloseButton

loginSpacer :: Component ()
loginSpacer = Atom {..}
  where

    tag = division

    styles = return ()

    element = do
      on XS (Flex.col 2.5)
      on SM (Flex.col 2.5)
      on MD (Flex.col 15)
      on LG (Flex.col 15)
      return ()

loginContent :: Component (Signal App IO ())
loginContent = Atom {..}
  where

    tag = division

    styles = do
      marginTop =: auto
      marginBottom =: auto
      bgColor =: azure

    element = do
      on XS (Flex.col 95)
      on SM (Flex.col 95)
      on MD (Flex.col 70)
      on MD (Flex.col 70)
      fst <$> listen E.click (const ()) listenOpts
      -- modalCloseButtonClicks <- embed loginHeader
      -- embed forms
      -- return modalCloseButtonClicks

modalCloseButton :: Component (Signal App IO ())
modalCloseButton = Atom {..}
  where

    tag = anchor

    styles = do
      float      =: right
      position   =: relative
      top        =: zero
      right      =: zero
      float      =: right

    element = do
      embed modalCloseGlyph
      fst <$> listen E.click (const ()) listenOpts

modalCloseGlyph = Atom {..}
  where

    tag = span

    styles = do
      color      =: hex 0xaaa
      fontSize   =: px 20

    element = do
      glyph gRemoveSign

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
    with modalName $ change $
      visibility =: visible
    void $ behavior' exits $ \r _ -> do
      with modalName $ change $
        visibility =: hidden
      unlistenEscs
      stop' closesToken
      stop' escapeToken
      Iron.end r

loginHeader :: Component (Signal App IO ())
loginHeader = Atom {..}
  where

    tag = division

    styles = do
      return ()

    element = do
      text "Log in or sign up"
      embed modalCloseButton

forms :: Component ()
forms = Atom {..}
  where

    tag = division

    styles = do
      return ()

    element = do
      embed loginForm
      embed signupForm

loginForm :: Component ()
loginForm = Atom {..}
  where

    tag = division

    styles = do
      return ()

    element = void $ do
      text "Left text"

signupForm :: Component ()
signupForm = Atom {..}
  where

    tag = division

    styles = do
      borderLeft =: spaces <| str (px 1) solid gray

    element = void $ do
      text "Right text"
