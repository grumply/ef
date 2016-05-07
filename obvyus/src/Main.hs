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
      embed primaryWidget
      style $ do
        height =: per 100
      -- embed secondaryWidget

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
      (hovers,_) <- listen E.mouseEnter id listenOpts
      (blurs,_)  <- listen E.blurEvent  id listenOpts
      (clicks,_) <- listen E.click id listenOpts
      setAttr xmlns "http://wwww.w3.org/2000/svg"
      setAttr width $ dec 24
      setAttr height $ dec 24
      svgEmbed pathZero
      svgEmbed pathOne
      svgEmbed lineOne
      svgEmbed lineTwo


d4 x1 y1 x2 y2 = str (dec x1) (dec y1) (dec x2) (dec y2)

data SVG self super result
  = SVGPath
    { path_d :: Str
    , path_styles :: Narrative '[Carbon] IO ()
    , path_element :: Narrative '[Hydrogen] (Narrative self super) result
    }
  | SVGLine
    { line_points :: (Double,Double,Double,Double)
    , line_styles :: Narrative '[Carbon] IO ()
    , line_element :: Narrative '[Hydrogen] (Narrative self super) result
    }

svgEmbed :: (Monad super, Lift IO super, Web :> self)
         => SVG self super result -> Narrative '[Hydrogen] (Narrative self super) result
svgEmbed SVGPath {..} = embed $ Atom "path" path_styles (setAttr "d" path_d >> path_element)
svgEmbed SVGLine {..} =
  let (x1,y1,x2,y2) = line_points
      points = do
        set A.x1 (dec x1)
        set A.y1 (dec y1)
        set A.x2 (dec x2)
        set A.y2 (dec y2)
  in embed $ Atom "line" line_styles (points >> line_element)

pathZero = SVGPath {..}
  where

    path_d = return ()

    path_styles = do
      fill =: hex 0xfff

    path_element = return ()

pathOne = SVGPath {..}
  where

    path_d = do
      moveTo 12.6 1.7
      hor 0
      arc 11 11 0 False False 0 22
      hor 0
      arc 11 11 0 False False 12.6 1.7
      close

    path_styles = do
      strokeWidth =: px 2

    path_element = do
      setAttr "transform" "translate(-0.7 -0.7)"

lineOne = SVGLine {..}
  where

    line_points = (5.1,12,18.9,12)

    line_styles = do
      fill             =: none
      stroke           =: hex 0x000
      strokeMiterlimit =: int 10
      strokeLinecap    =: round_
      strokeWidth      =: px 4

    line_element = return ()


lineTwo = SVGLine {..}
  where

    line_points = (11.9,4.9,12,19)

    line_styles = do
      fill =: none
      stroke =: hex 0x000
      strokeMiterlimit =: int 10

    line_element = return ()

moveTo x y = restr $ do
  string "M"
  space
  dec x
  space
  dec y
  space

hor y = restr $ do
  string "H"
  space
  dec y
  space

ver x = restr $ do
  string "V"
  space
  dec x
  space

lineTo x y = restr $ do
  string "L"
  space
  dec x
  space
  dec y
  space

arc rx ry xrot large sweep x y = restr $ do
  string "A"
  space
  dec rx
  space
  dec ry
  space
  dec xrot
  space
  if large then one else zero
  space
  if sweep then one else zero
  space
  dec x
  space
  dec y
  space

close = str "Z"

cubic x1 y1 x2 y2 x y = restr $ do
  string "C"
  space
  dec x1
  space
  dec y1
  comma
  space
  dec x2
  space
  dec y2
  comma
  space
  dec x
  space
  dec y
  space

connectedTo x y = restr $ do
  string "T"
  space
  dec x
  space
  dec y
  space

smooth x2 y2 x y = restr $ do
  string "S"
  space
  dec x2
  space
  dec y2
  comma
  space
  dec x
  space
  dec y
  space

quadratic x1 y1 x y = restr $ do
  string "Q"
  space
  dec x1
  space
  dec y1
  comma
  space
  dec x
  space
  dec y
  space



-- pathA = Atom {..}
--   where

--     tag = 

--     styles = do
--       fill =: black

--     element = do
--       setAttr "d" "M12.6,1.7h0a11,11,0,0,0,0,22h0A11,11,0,0,0,12.6,1.7Z"
--       setAttr "transform" "translate(-0.7 -0.7)"

-- pathB = Atom {..}
--   where

--     tag = path

--     styles = do
--       fill =: black

--     element = do
--       setAttr "d" (m (ds 12.6 1.7))

