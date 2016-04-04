{-# language RecordWildCards #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language PolyKinds #-}
module Main where

import Ef
import Ef.Single
import Data.Promise

import Iron

import Carbon
import Helium
import Hydrogen
import Oxygen
import Magnesium
import Silicon

import Flex

import Components
import Component.Body
import Component.Modal
import Component.Util
import Component.Login

import Model

import View.About
import View.Contact
import View.Privacy
import View.Interesting
import View.Provacative

import App

import Control.Monad

import qualified GHCJS.DOM.Element as E

main :: IO ()
main = run Config{..}
    where
        routes = do
            path "/interesting" $ do
              interesting $ do
                return ()

            path "/about" $ do
              about $ do
                return ()

            path "/contact" $ do
              contact $ do
                return ()

            path "/privacy" $ do
              privacy $ do
                return ()

            provacative $ do
              return ()

        prime base = return (app base)

        build = setup

setup :: Narrative App IO ()
setup = do
  void $ with fusion $ do
    embed modal
    modalCloseClicks <- super $ with modalName $ do
      example
      fst <$> listen E.click (const ()) listenOpts
    child "div" Nothing $ do
      style $ do
        height =: per 100
      loginClicks <- embed bodyTop
      embed bodyBottom
      loginModalHandler loginClicks modalCloseClicks
  void addFusion

example =
 void $ do
  child division Nothing $ style (Flex.col 2.5)
  child division Nothing $ do
    style $ do
      marginTop =: auto
      marginBottom =: auto
      bgColor       =: black
      Flex.col 95
    text "test"
  child division Nothing $ style (Flex.col 2.5)
