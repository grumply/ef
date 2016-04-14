{-# language RecordWildCards #-}
{-# language DataKinds #-}
module Component.Modal where

import Ef

import App

import Carbon as CSS hiding (text)
import Hydrogen as HTML
import Helium as Str
import Neon as Glyph
import Oxygen
import Magnesium
import Silicon
import Iron

import Flex

import Components
import Component.Util

import Control.Monad
import Prelude hiding (span)

modal :: Component ()
modal = Named {..}
  where

    tag = division

    name = modalName

    styles = do
      Flex.row
      position   =: fixed
      bgColor    =: rgba(0,0,0,0.4)
      height     =: per 100
      width      =: per 100
      visibility =: hidden

    element =
      return ()
