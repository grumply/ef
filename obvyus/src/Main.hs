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

content = create "div" (Just "content") (return ())

main :: IO ()
main = run Config{..}
    where
        routes = do
            url <- getUrl
            liftIO' $ print url
            path "/test" $ do
                liftIO' $ print "in /test"
                dispatch $ with "content" $
                    child "p" Nothing $ setText "test"
            path "/name/:name" $ do
                liftIO' $ print "in /name/:name"
                Just name <- "name"
                dispatch $ with "content" $ do
                    deleteChildren
                    child "p" Nothing $ setText $ "Hello, " ++ name
            dispatch $ with "content" $ do
                liftIO' $ print "in default"
                deleteChildren
                child "p" Nothing $ setText "Default"
                
        prime base = return (listings *:* obvyus *:* menu *:* base,())

        build _ = do
            create_ "div" (Just "lotus") (return ())
            with "lotus" $ do
                child "div" Nothing $ do
                    containerFluid
                    appendMenu
                    divider
                    content
                    divider
                    appendFooter
            addLotus

        drive = blockingDriver
