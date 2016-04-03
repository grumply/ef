{-# language NoMonomorphismRestriction #-}
module Flex
  ( container
  , Flex.row, Flex.rowReverse
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

import Control.Monad

container = do
    marginRight  =: str auto
    marginLeft   =: str auto
    paddingRight =: px 16
    paddingLeft  =: px 16

row = do
    boxSizing           =: borderBox

    display             =: webkitFlex
    display             +: msFlexBox
    display             +: webkitBox
    display             +: flex

    webkitBoxFlex       =: str zero
    webkitFlex          =: vals <| str zero one auto
    msFlex              =: vals <| str zero one auto
    flex                =: vals <| str zero one auto

    webkitBoxOrient     =: horizontal
    webkitBoxDirection  =: normal

    webkitFlexDirection =: CSS.row
    msFlexDirection     =: CSS.row
    flexDirection       =: CSS.row

    webkitFlexWrap      =: wrap
    msFlexWrap          =: wrap
    flexWrap            =: wrap

    marginLeft          =: px (-8)
    marginRight         =: px (-8)

rowReverse = do
    webkitFlexDirection =: CSS.rowReverse
    msFlexDirection     =: CSS.rowReverse
    webkitBoxOrient     =: horizontal
    webkitBoxDirection  =: CSS.reverse
    flexDirection       =: CSS.rowReverse

colReverse = do
    webkitFlexDirection =: CSS.columnReverse
    msFlexDirection     =: CSS.columnReverse
    webkitBoxOrient     =: vertical
    webkitBoxDirection  =: CSS.reverse
    flexDirection       =: CSS.columnReverse

flexBaseColumnStyles = do
    boxSizing     =: borderBox
    webkitBoxFlex =: zero
    webkitFlex    =: spaces <| str zero zero auto
    msFlex        =: spaces <| str zero zero auto
    flex          =: spaces <| str zero zero auto
    paddingRight  =: px 8
    paddingLeft   =: px 8

col percent = do
  let w = per percent
  msFlexPreferredSize =: w
  webkitFlexBasis     =: w
  flexBasis           =: w
  maxWidth            =: w
  flexBaseColumnStyles

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
