{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language NoMonomorphismRestriction #-}
module Utility where

import Ef

import Carbon
import Helium
import Hydrogen
import Iron

import Flex

import Modal

divider = Atom {..}
  where

    tag = division

    styles =
      Flex.row

    element = do
      embed spacer
      embed rule
      embed spacer

spacer = Atom {..}
  where

    tag = division

    styles =
      Flex.col 10

    element =
      return ()

rule = Atom {..}
  where

    tag = hr

    styles =  do
      Flex.col 80
      border  =: zero
      height  =: px 1
      margin  =: px2 5 0
      bgImage =: "linear-gradient(to right,rgba(0,0,0,0),rgba(0,0,0,0.75),rgba(0,0,0,0))"

    element = return ()

timesNewRoman weight_ color_ size_ =
    font "\"Times New Roman\", Serif"
         weight_
         color_
         size_

helveticaNeue weight_ color_ size_ =
    font "\"Helvetica Neue\",Helvetica,Arial"
         weight_
         color_
         size_

