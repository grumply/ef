module Page where

import Ef

import Components
import App

import Helium
import Hydrogen
import Carbon
import Magnesium

import Control.Monad

changeMenuHighlights :: Str -> Str -> HTML ()
changeMenuHighlights iColor pColor =
    void $ super $ do
        with "InterestingLink" $ change $ color =: iColor
        with "ProvacativeLink" $ change $ color =: pColor


page :: Str -> Str -> HTML () -> Route ()
page iColor pColor c =
    dispatch $ with mainContentName $ do
        changeMenuHighlights iColor pColor
        deleteChildren
        c
