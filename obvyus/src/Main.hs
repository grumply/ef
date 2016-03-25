{-# OPTIONS_GHC -O0 #-}
{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
module Main where

import Ef
import Lotus

import Obvyus
import Menu
import Listing
import Listings

{-|
1. Create a menu component that can be reused across pages. 
2. Create a listings component that can be set to update across page boundaries in the background.
    a. Should support modifications like rearranging, vote count updates, and replacement
3. Set up routing to reuse the menu component
-}

-- data Config self methods super primeResult buildResult = Config

data Hole

main :: IO ()
main = run Config{..}
    where
        routes = do
            path "/test1" $ page $ do
                with "lotus" $ do
                    deleteChildren
                    append "p" Nothing $ do
                        child "a" Nothing $ do
                            setAttr "href" "#/test2"
                            setText "test2"
            path "/test2" $ page $ do
                with "lotus" $ do
                    deleteChildren
                    append "p" Nothing $ do
                        child "a" Nothing $ do
                            setAttr "href" "#/test1"
                            setText "test1"
            page $
                with "lotus" $ append "p" Nothing $ setText "Default"

        prime base = return (listings *:* obvyus *:* menu *:* base,())

        build = return

        drive = blockingDriver
