{-# language OverloadedStrings #-}
{-# language ImplicitParams #-}
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
import Attributes as A
import Elements as E

import Flex

import Control.Monad

import qualified GHCJS.DOM.Element as E

import Prelude hiding (span)


main = run Config{..}
  where

    prime = return

    build = void $ with fusion $ do
      html primaryWidget
      style $ do
        height =: per 100
      -- embed secondaryWidget

    routes = dispatch $ return ()

primaryWidget = Atom {..}
  where

    tag = division

    styles = do
      position =: absolute
      display =: block
      top =: px 128
      left =: px 64

    element = do
      svg addButton

addButton = SVG {..}
  where

    svgTag = svg_

    svgStyles = return ()

    svgElement = do
      (mouseInto,_)  <- listen E.mouseEnter id listenOpts
      (mouseOutof,_) <- listen E.mouseLeave  id listenOpts
      (clicks,_)     <- listen E.click id listenOpts
      svgDimensions 24 24
      definitions $ do
        svg circleGradient
      svg boundingCircle
      -- svg horLine
      -- svg verLine

circleGradient = NamedSVG {..}
  where

    svgName = "addButtonCircleGradient"

    svgTag = E.radialGradient

    svgStyles = return ()

    svgElement = do
      svg (gradientStop 80 blue)
      svg (gradientStop 90 orange)

gradientStop p c = SVG {..}
  where

    svgTag = stop_

    svgStyles = return ()

    svgElement = do
      svgSet offset (per p)
      svgSet stopColor c

boundingCircle = Circle {..}
  where

    circleCenter = (12,12)

    circleRadius = 10

    circleStyles = return ()

    circleElement = do
      svgSet fill (url (individual "addButtonCircleGradient"))
