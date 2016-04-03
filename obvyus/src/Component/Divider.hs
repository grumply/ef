{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
module Component.Divider where

import Carbon
import Helium
import Hydrogen
import Iron
import Oxygen

import App

import Flex

divider :: Component ()
divider = Atom {..}
  where

    tag = division

    styles =
      Flex.row

    element = do
      embed spacer
      embed rule
      embed spacer

spacer :: Component ()
spacer = Atom {..}
  where

    tag = division

    styles =
      Flex.col 10

    element =
      return ()

rule :: Component ()
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
