{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
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

lotusStyles = do
    minHeight =: per 100
    height    =: render auto important
    height    +: per 100
    flexibleContainer

content :: String
content = "content"

contentContainer = child "div" (Just content) (return ())

changeMenuHighlights iColor pColor =
    super $ do
        with "InterestingLink" $ do
            liftIO $ print $ "coloring interesting " ++ iColor
            change $ color =: iColor
        with "ProvacativeLink" $ do
            liftIO $ print $ "coloring provoacative " ++ pColor
            change $ color =: pColor


main :: IO ()
main = run Config{..}
    where
        routes = do
            path "/interesting" $ do
                dispatch $ with content $ do
                    liftIO $ print "in interesting"
                    changeMenuHighlights black gray
                    deleteChildren

            path "/login" $ do
                dispatch $ with content $ do
                    liftIO $ print "in login"
                    changeMenuHighlights gray gray
                    deleteChildren

            path "/about" $ do
                dispatch $ with content $ do
                    liftIO $ print "in about"
                    changeMenuHighlights gray gray
                    deleteChildren

            path "/contact" $ do
                dispatch $ with content $ do
                    liftIO $ print "in contact"
                    changeMenuHighlights gray gray
                    deleteChildren

            path "/privacy" $ do
              dispatch $ with content $ do
                    liftIO $ print "in privacy"
                    changeMenuHighlights gray gray
                    deleteChildren

            dispatch $ do
                with content $ do
                    liftIO $ print "in default; provacative"
                    changeMenuHighlights gray black
                    p <- deleteChildren
                    demand p
                    liftIO $ print "after deleteChildren"

        prime base = return (listings *:* obvyus *:* mkMenu *:* base,())

        build _ = do
            with lotus $ do
                style $ do
                    height =: per 100
                    flexibleContainer
                div_ $ do
                    style $ do
                        minHeight =: per 100
                        height =: render auto important
                        height +: per 100
                        margin =: render zero auto (ems (-4))
                    hat
                    contentContainer
                    pushDiv
                footer
            addNelumbo

        drive = blockingDriver

pushDiv = child "div" Nothing $ do
    style $ do
        height =: ems 4
