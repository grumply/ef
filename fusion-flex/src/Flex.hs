{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}
module Flex
  ( flexible, responsive
  , transitionFlexWidth
  , Flex.row, Flex.rowReverse
  , Flex.column
  , col, colReverse
  , Flex.start, Flex.end
  , Flex.center, Flex.middle
  , Flex.around, Flex.between
  , Flex.first, Flex.last
  ) where

import Ef

import Helium
import Iron
import Carbon as CSS
import Oxygen
import Silicon

import Attributes as CSS

import Control.Monad

flexible f xs sm md lg = void $ do
  on XS (f xs)
  on SM (f sm)
  on MD (f md)
  on LG (f lg)

responsive f = flexible (f =:)

-- a column-oriented flexible layout
--   horizontal box orientation
--   normal top-to-bottom directional layout
--   overflow wraps
column = do
    boxSizing =: borderBox

    display =: webkitFlex
    display +: msFlexBox
    display +: webkitBox
    display +: flex

    webkitBoxFlex =: zero
    webkitFlex =: vals <| str zero one auto
    msFlex =: vals <| str zero one auto
    flex =: vals <| str zero one auto

    webkitFlexDirection =: CSS.column
    msFlexDirection =: CSS.column
    flexDirection =: CSS.column

    Flex.orient Horizontal
    Flex.direction Normal
    Flex.wrap Wrap

data Orientation = Vertical | Horizontal | InlineAxis | BlockAxis

orient Vertical = webkitBoxOrient =: vertical
orient Horizontal = webkitBoxOrient =: horizontal
orient InlineAxis = webkitBoxOrient =: string "inline-axis"
orient BlockAxis = webkitBoxOrient =: string  "block-axis"

data Direction = Normal | Reverse

direction Normal = webkitBoxDirection =: normal
direction Reverse = webkitBoxDirection =: CSS.reverse

data Wrap = Wrap | NoWrap | WrapReverse

wrap Wrap = do
  webkitFlexWrap =: CSS.wrap
  msFlexWrap =: CSS.wrap
  flexWrap =: CSS.wrap
wrap NoWrap = do
  webkitFlexWrap =: CSS.nowrap
  msFlexWrap =: CSS.nowrap
  flexWrap =: CSS.nowrap
wrap WrapReverse = do
  webkitFlexWrap =: wrapreverse
  msFlexWrap =: wrapreverse
  flexWrap =: wrapreverse

-- a row-oriented flexible layout
--  horizontal box orientation
--  normal left-to-right directional layout
--  overflow wraps
row = do
    boxSizing           =: borderBox

    display             =: webkitFlex
    display             +: msFlexBox
    display             +: webkitBox
    display             +: flex

    webkitBoxFlex       =: zero
    webkitFlex          =: vals <| str zero one auto
    msFlex              =: vals <| str zero one auto
    flex                =: vals <| str zero one auto

    webkitFlexDirection =: CSS.row
    msFlexDirection     =: CSS.row
    flexDirection       =: CSS.row

    Flex.orient Horizontal
    Flex.direction Normal
    Flex.wrap Wrap

rowReverse = do
    webkitFlexDirection =: CSS.rowReverse
    msFlexDirection     =: CSS.rowReverse
    webkitBoxOrient     =: horizontal
    webkitBoxDirection  =: CSS.reverse
    flexDirection       =: CSS.rowReverse

transitionFlexWidth dur easing = do
  transition =: spaces <| str msFlexPreferredSize dur easing
  transition =: spaces <| str webkitFlexBasis dur easing
  transition =: spaces <| str flexBasis dur easing
  transition =: spaces <| str maxWidth dur easing

flexWidth percent = do
  let w = per percent
  msFlexPreferredSize =: w
  webkitFlexBasis     =: w
  flexBasis           =: w
  maxWidth            =: w

col percent = do
  flexWidth percent
  boxSizing           =: borderBox
  webkitBoxFlex       =: zero
  webkitFlex          =: spaces <| str zero zero auto
  msFlex              =: spaces <| str zero zero auto
  flex                =: spaces <| str zero zero auto

colReverse = do
    webkitFlexDirection =: CSS.columnReverse
    msFlexDirection     =: CSS.columnReverse
    webkitBoxOrient     =: vertical
    webkitBoxDirection  =: CSS.reverse
    flexDirection       =: CSS.columnReverse

start = do
    webkitJustifyContent =: flexStart
    msFlexPack           =: CSS.start
    webkitBoxPack        =: CSS.start
    justifyContent       =: flexStart

    -- I would like to have textAlign =: start,
    -- but IE10/Edge do not support it. This
    -- breaks rtl sites, sadly. Write to your
    -- local or state Microsoft representative.
    -- textAlign            =: start
    textAlign            =: left

center = do
    webkitJustifyContent =: CSS.center
    msFlexPack           =: CSS.center
    webkitBoxPack        =: CSS.center
    justifyContent       =: CSS.center
    textAlign            =: CSS.center

end = do
    webkitJustifyContent =: flexEnd
    msFlexPack           =: CSS.end
    webkitBoxPack        =: CSS.end
    justifyContent       =: flexEnd

    -- I would like to have textAlign =: end,
    -- but IE10/Edge do not support it. This
    -- breaks rtl sites, sadly. Write to your
    -- local or state Microsoft representative.
    -- textAlign            =: end
    textAlign            =: right

top = do
    webkitBoxAlign   =: CSS.start
    webkitAlignItems =: flexStart
    msFlexAlign      =: CSS.start
    alignItems       =: flexStart

middle = do
    webkitAlignItems =: CSS.center
    msFlexAlign      =: CSS.center
    webkitBoxAlign   =: CSS.center
    alignItems       =: CSS.center

bottom = do
    webkitAlignItems =: flexEnd
    msFlexAlign      =: CSS.end
    webkitBoxAlign   =: CSS.end
    alignItems       =: flexEnd

around = do
    webkitJustifyContent =: spaceAround
    msFlexPack           =: distribute
    justifyContent       =: spaceAround

between = do
    webkitJustifyContent =: spaceBetween
    msFlexPack           =: justify
    webkitBoxPack        =: justify
    justifyContent       =: spaceBetween

first = do
    webkitOrder           =: int (-1)
    msFlexOrder           =: int (-1)
    webkitBoxOrdinalGroup =: zero
    order                 =: int (-1)

last = do
    webkitOrder           =: one
    msFlexOrder           =: one
    webkitBoxOrdinalGroup =: two
    order                 =: one
