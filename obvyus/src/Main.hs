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

import Model

import View.About
import View.Contact
import View.Privacy
import View.Interesting
import View.Provacative

import App

import Control.Monad

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
    style $ do
      height =: per 100
      Flex.container
    embed bodyTop
    embed bodyBottom
  void addFusion
