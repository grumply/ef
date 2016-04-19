{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}
module Flex
  ( flexible, responsive
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

flexible f xs sm md lg = void $ do
  on XS (f xs)
  on SM (f sm)
  on MD (f md)
  on LG (f lg)

responsive = flexible

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

rowReverse = do
    webkitFlexDirection =: CSS.rowReverse
    msFlexDirection     =: CSS.rowReverse
    webkitBoxOrient     =: horizontal
    webkitBoxDirection  =: CSS.reverse
    flexDirection       =: CSS.rowReverse

col percent = do
  let w = per percent
  msFlexPreferredSize =: w
  webkitFlexBasis     =: w
  flexBasis           =: w
  maxWidth            =: w
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
