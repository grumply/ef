{-# language OverloadedStrings #-}
module Modal where

import Ef

import Carbon as CSS
import Helium
import Hydrogen
import Iron

import Flex

modal nameBase els = Named {..}
  where

    tag = division

    name = nameBase ++ "Modal"

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
      visibility    =: visible

    element = do
      super $ stylePage (individual name) $ do
        opacity =: zero
        pointerEvents =: none
      super $ stylePage (target (individual name)) $ do
        opacity =: one
        pointerEvents =: auto
      embed $ modalContent name els

modalContent nameBase els = Named {..}
  where

    tag = division

    name = nameBase ++ "Content"

    styles = do
      position     =: relative
      margin       =: spaces <| str (per 10) auto none auto
      borderRadius =: px 10
      paddingTop   =: px 20
      background   =: linearGradient <| str (string "aliceblue") azure

    element = do
      flexible col 85 85 50 50
      _ <- embed $ modalClose nameBase
      els

modalClose nameBase = Named {..}
  where

    name = nameBase ++ "Close"

    tag = anchor

    styles = do
      fontFamily     =: "Arial, Helvetica, sans-serif"
      color          =: hex 0xFFFFFF
      lineHeight     =: px 25
      position       =: absolute
      right          =: px 10
      textAlign      =: CSS.center
      top            =: px 10
      width          =: px 24
      textDecoration =: none
      fontWeight     =: CSS.bold
      borderRadius   =: px 12
      zIndex         =: int 9999
      boxShadow      =: str (px 1) (px 1) (px 3) (hex 0x000)

    element = do
      super $ stylePage (individual name) $
        background =: hex 0x606060
      super $ stylePage (hover (individual name)) $
        background =: hex 0x00d9ff
      _ <- href "close"
      setText "X"
