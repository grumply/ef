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
        routes = return ()

        prime base = return (listings *:* obvyus *:* menu *:* base,())

        build = return

        drive = blockingDriver
