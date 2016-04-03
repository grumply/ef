{-# language OverloadedStrings #-}
module Component.Util where

import Carbon
import Helium

import App

footSize :: Double
footSize = 3

timesNewRoman :: Str -> Str -> Str -> CSS ()
timesNewRoman weight_ color_ size_ =
    font "\"Times New Roman\", Serif"
         weight_
         color_
         size_

helveticaNeue :: Str -> Str -> Str -> CSS ()
helveticaNeue weight_ color_ size_ =
    font "\"Helvetica Neue\",Helvetica,Arial"
         weight_
         color_
         size_
