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

import Fusion

import Attributes as A

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

    webkitFlexDirection =: A.column
    msFlexDirection =: A.column
    flexDirection =: A.column

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
direction Reverse = webkitBoxDirection =: A.reverse

data Wrap = Wrap | NoWrap | WrapReverse

wrap Wrap = do
  webkitFlexWrap =: A.wrap
  msFlexWrap =: A.wrap
  flexWrap =: A.wrap
wrap NoWrap = do
  webkitFlexWrap =: A.nowrap
  msFlexWrap =: A.nowrap
  flexWrap =: A.nowrap
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

    webkitFlexDirection =: A.row
    msFlexDirection     =: A.row
    flexDirection       =: A.row

    Flex.orient Horizontal
    Flex.direction Normal
    Flex.wrap Wrap

rowReverse = do
    webkitFlexDirection =: A.rowReverse
    msFlexDirection     =: A.rowReverse
    webkitBoxOrient     =: horizontal
    webkitBoxDirection  =: A.reverse
    flexDirection       =: A.rowReverse

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
    webkitFlexDirection =: A.columnReverse
    msFlexDirection     =: A.columnReverse
    webkitBoxOrient     =: vertical
    webkitBoxDirection  =: A.reverse
    flexDirection       =: A.columnReverse

start = do
    webkitJustifyContent =: flexStart
    msFlexPack           =: A.start
    webkitBoxPack        =: A.start
    justifyContent       =: flexStart

    -- I would like to have textAlign =: start,
    -- but IE10/Edge do not support it. This
    -- breaks rtl sites, sadly. Write to your
    -- local or state Microsoft representative.
    -- textAlign            =: start
    textAlign            =: left

center = do
    webkitJustifyContent =: A.center
    msFlexPack           =: A.center
    webkitBoxPack        =: A.center
    justifyContent       =: A.center
    textAlign            =: A.center

end = do
    webkitJustifyContent =: flexEnd
    msFlexPack           =: A.end
    webkitBoxPack        =: A.end
    justifyContent       =: flexEnd

    -- I would like to have textAlign =: end,
    -- but IE10/Edge do not support it. This
    -- breaks rtl sites, sadly. Write to your
    -- local or state Microsoft representative.
    -- textAlign            =: end
    textAlign            =: right

top = do
    webkitBoxAlign   =: A.start
    webkitAlignItems =: flexStart
    msFlexAlign      =: A.start
    alignItems       =: flexStart

middle = do
    webkitAlignItems =: A.center
    msFlexAlign      =: A.center
    webkitBoxAlign   =: A.center
    alignItems       =: A.center

bottom = do
    webkitAlignItems =: flexEnd
    msFlexAlign      =: A.end
    webkitBoxAlign   =: A.end
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
