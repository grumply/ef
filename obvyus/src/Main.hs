{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language NoMonomorphismRestriction #-}
{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Ef

import Flowers

import Obvyus
import Menu
import Listing
import Listings

import Data.Promise

import Control.Monad
import Prelude hiding (div)
import qualified Data.Map as Map

appendMenu = do
    menu <- super $ do
        initialized <- menuInitialized
        unless initialized initializeMenu
        menuNode
    nodeAppend menu (return ())

appendFooter = do
    return ()

content = create "div" (Just "content") $ do
    responsive 12 12 10 10

changeMenuHighlights iColor pColor =
    super $ do
        with "InterestingLink" $ style $ "color" =: iColor
        with "ProvacativeLink" $ style $ "color" =: pColor

main :: IO ()
main = run Config{..}
    where
        routes = do
            path "/interesting" $ dispatch $ with "content" $ do
                changeMenuHighlights "black" "gray"
                deleteChildren
                p_ $ text "interesting page"

            path "/login" $ dispatch $ with "content" $ do
                changeMenuHighlights "gray" "gray"
                deleteChildren
                p_ $ text "login form"

            dispatch $ with "content" $ do
                changeMenuHighlights "gray" "black"
                deleteChildren
                p_ $ text "default page; provacative"

        prime base = return (listings *:* obvyus *:* mkMenu *:* base,())

        build _ = do
            create_ "div" (Just "lotus") (return ())
            with "lotus" $
                div_ $ do
                    style containerFluid
                    appendMenu
                    divider
                    content
                    divider
                    appendFooter
            addNelumbo

        drive = blockingDriver
