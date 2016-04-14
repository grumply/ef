{-# language RecordWildCards #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language PolyKinds #-}
{-# language OverloadedStrings #-}
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

defaultRouter =  do
  path "/loginModal" $ do
    return ()
    
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


main :: IO ()
main = run Config{..}
    where
        routes = defaultRouter
       
        prime base = return (app base)

        build = setup

setup :: Narrative App IO ()
setup = do
  void $ with fusion $ do
    child "div" (Just "modalDialog") $ do
      child "div" (Just "close") $ do
        style $ do
            background     =: hex 0x606061
            color          =: hex 0xFFFFFF
            lineHeight     =: px 25
            position       =: absolute
            right          =: px (-12)
            textAlign      =: Carbon.center
            top            =: px (-10)
            width          =: px 24
            textDecoration =: none
            fontWeight     =: bold
            borderRadius   =: px 12
            boxShadow      =: str (px 1) (px 1) (px 3) (hex 0x000)
      child "h2" Nothing $ do
        text "Test"
      style $ do
        position      =: fixed
        fontFamily    =: "Arial, Helvetica, sans-serif"
        top           =: zero
        right         =: zero
        bottom        =: zero
        left          =: zero
        background    =: rgba(0,0,0,0.8)
        zIndex        =: int 99999
        opacity       =: zero
        transition    =: spaces <| str opacity (ms 400) easeIn
        pointerEvents =: none
    super $ styleGlobal (target (classified "modalDialog")) $ do
      opacity       =: one
      pointerEvents =: auto
    super $ styleGlobal (hover (classified "close")) $ do
      background =: hex 0x00d9ff
    super $ styleGlobal (parents <| str (classified "modalDialog") <| string division) $ do
      width        =: px 400
      position     =: relative
      margin       =: str (per 10) auto
      padding      =: px4 5 20 13 20
      borderRadius =: px 10
      background   =: linearGradient <| commas <| str (hex 0xfff) (hex 0x999)
    child "div" Nothing $ do
      style $ do
        minHeight =: per 100
        height    =: spaces <| str auto important
        height    +: per 100
      embed bodyTop
      embed bodyBottom



-- spacer = Atom {..}
--   where

--     tag = division

--     styles = return ()

--     element = do
--       on XS (Flex.col 2.5)
--       on SM (Flex.col 2.5)
--       on MD (Flex.col 15)
--       on LG (Flex.col 15)

-- content = Atom {..}
--   where

--     tag = division

--     styles = do
--       marginTop =: auto
--       marginBottom =: auto
--       bgColor =: black

--     element = do
--       on XS (Flex.col 95)
--       on SM (Flex.col 95)
--       on MD (Flex.col 70)
--       on LG (Flex.col 70)
--       text "test"

-- example =
--  void $ do
--   child division Nothing $ style (Flex.col 2.5)
--   child division Nothing $ do
--     style $ do
--       marginTop =: auto
--       marginBottom =: auto
--       bgColor       =: black
--       Flex.col 95
--     text "test"
--   child division Nothing $ style (Flex.col 2.5)
