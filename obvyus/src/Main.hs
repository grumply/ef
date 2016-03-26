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

import Lily
import Lily.Examples
import Control.Monad
import Prelude hiding (div)
{-|
1. Create a menu component that can be reused across pages. 
2. Create a listings component that can be set to update across page boundaries in the background.
    a. Should support modifications like rearranging, vote count updates, and replacement
3. Set up routing to reuse the menu component
-}

-- data Config self methods super primeResult buildResult = Config
test i = do
  (elems,_) <- create "div" Nothing $ do
      replicateM_ (read i) $
          child "div" Nothing $ do
              containerFluid
              test1
  with "lotus" $ do
      nodeAppend elems (return ())
  return ()

main :: IO ()
main = run Config{..}
    where
        routes = do
          path "/:count" $ do
              Just i <- "count"
              page $ with "lotus" $ replicateM_ (read i) $ child "div" Nothing $ do
                  containerFluid
                  test1

        prime base = return (listings *:* obvyus *:* menu *:* base,())

        build = return

        drive = blockingDriver
