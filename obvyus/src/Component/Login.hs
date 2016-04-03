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

    styles = do
      borderRadius =: px 15
      backgroundColor =: azure
      overflow =: hidden

    element = do
      on XS (Flex.col 90)
      on SM (Flex.col 90)
      on MD (Flex.col 60)
      on LG (Flex.col 60)
      embed loginContent
      -- embed modalCloseButton
      fst <$> listen E.click (const ()) listenOpts

loginContent :: Component ()
loginContent = Atom {..}
  where

    tag = division

    styles = do
      return ()

    element = do
      embed loginHeader
      embed forms

modalCloseButton :: Component (Signal App IO ())
modalCloseButton = Atom {..}
  where

    tag = span

    styles = do
      color      =: hex 0xaaa
      float      =: right
      position   =: relative
      top        =: zero
      right      =: zero
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
    with modalName $ change $
      visibility =: visible
    void $ behavior' exits $ \r _ -> do
      with modalName $ change $
        visibility =: hidden
      unlistenEscs
      stop' closesToken
      stop' escapeToken
      Iron.end r

loginHeader :: Component ()
loginHeader = Atom {..}
  where

    tag = division

    styles = do
      return ()

    element = void $ do
      text "Log in or sign up"

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
