{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language NoMonomorphismRestriction #-}
{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
module Main where

import Ef
import Lotus

import Obvyus
import Menu
import Listing
import Listings

import Data.Promise

import Lily
import Lily.Examples
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

changeMenuHighlights iColor pColor lColor =
    super $ do
        with "InterestingLink" $ replaceStyles [style "color" iColor]
        with "ProvacativeLink" $ replaceStyles [style "color" pColor]
        with "LoginLink" $ replaceStyles [style "color" lColor]

main :: IO ()
main = run Config{..}
    where
        routes = do
            path "/interesting" $ dispatch $ with "content" $ do
                changeMenuHighlights "black" "gray" "gray"
                deleteChildren
                child "p" Nothing $ do
                    setText "interesting page"

            path "/login" $ dispatch $ with "content" $ do
                changeMenuHighlights "gray" "gray" "black"
                deleteChildren
                child "p" Nothing $ do
                    setText "login form"

            dispatch $ with "content" $ do
                changeMenuHighlights "gray" "black" "gray"
                deleteChildren
                child "p" Nothing $ do
                    setText "default page; provacative"
        
        prime base = return (listings *:* obvyus *:* menu *:* base,())

        build _ = do
            create_ "div" (Just "lotus") (return ())
            with "lotus" $ do
                child "div" Nothing $ do
                    containerFluid
                    child "div" Nothing $ do
                        row
                        child "div" Nothing $ do
                            center
                            responsive 12 12 10 10
                            appendMenu
                            divider
                            content
                            divider
                            appendFooter
            addLotus

        drive = blockingDriver
