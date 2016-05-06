{-# language OverloadedStrings #-}
module Main where

import Ef
import Ef.Bidir
import Data.Promise

import Iron

import Carbon as CSS
import qualified Dicarbon
import Helium
import Hydrogen as HTML
import Oxygen
import Magnesium
import Silicon
import Neon

import Flex

import Control.Monad

import qualified GHCJS.DOM.Element as E

import Prelude hiding (span)


main = run Config{..}
  where

    prime = return

    build = void $ with fusion $ do
      embed primaryWidget
      embed secondaryWidget

    routes = dispatch $ return ()


primaryWidget = Atom {..}
  where

    tag = division

    styles = do
      position =: absolute
      top =: px 128
      left =: px 64

    element = do
      embed addMenuItem

addMenuItem = Atom {..}
  where

    tag = svg

    styles = return ()

    element = do
      (hovers,_) <- listen E.hover id listenOpts
      (blurs,_)  <- listen E.blur  id listenOpts
      (clicks,_) <- listen E.click id listenOpts
      viewBox 0 0 24 24
      embed pathA

pathA = Atom {..}
  where

    tag = path

    styles = do
      fill =: black

    element = do
      setAttr "d" "M12.6,1.7h0a11,11,0,0,0,0,22h0A11,11,0,0,0,12.6,1.7Z"
      setAttr "transform" "translate(-0.7 -0.7)"

pathB = Atom {..}
  where

    tag = path

    styles = do
      fill =: black

    element = do
      setAttr "d" (m (ds 12.6 1.7))
moveTo x y = str "M" (commas <| (dec x) (dec y))
lineTo x y = spaces <| str "L" (dec x) (dec y)
hor x = spaces <| str "H" (dec x)
ver y = spaces <| str "V" (dec y)

viewBox x0 y0 x1 x1 = setAttr "viewBox" (spaces <| str (int x0) (int y0) (int x1) (int y1))

{-
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <style>
      .a{fill:#fff;}.b,.c{fill:none;stroke:#000;stroke-miterlimit:10;}.b{stroke-width:2px;}.c{stroke-linecap:round;stroke-width:4px;}
    </style>
  </defs>
  <title>obvyus_builder_add_button</title>
  <path class="a" d= transform="translate(-0.7 -0.7)"/>
  <path class="b" d="M12.6,1.7h0a11,11,0,0,0,0,22h0A11,11,0,0,0,12.6,1.7Z" transform="translate(-0.7 -0.7)"/>
  <line class="c" x1="5.1" y1="12" x2="18.9" y2="12"/>
  <line class="c" x1="11.9" y1="4.9" x2="12" y2="19"/>
</svg>
-}
