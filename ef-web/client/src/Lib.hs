{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Lib
    ( runClient
    ) where

import Ef hiding (lift)
import Ef.IO

import Web

import Prelude hiding (log)

foreign import javascript unsafe
    "console.log($1);"
    consoleLog :: JSVal -> IO ()

log xs = consoleLog =<< toJSVal xs

runClient :: IO ()
runClient = do
    window <- initWindow
    let obj = Object $ window *:* Empty
    obj $. do
        ws <- sequence [screenWidth,screenAvailWidth
                       ,windowOuterWidth,windowInnerWidth
                       ,bodyClientWidth,bodyOffsetWidth
                       ,screenX
                       ]
        hs <- sequence [screenHeight,screenAvailHeight
                       ,windowOuterHeight,windowInnerHeight
                       ,bodyClientHeight,bodyOffsetHeight
                       ,screenY
                       ]
        io $ do
            log ws
            log hs
    return ()
