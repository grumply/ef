{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Lib
    ( runClient
    ) where

import Ef
import Ef.IO

import Control.Concurrent
import System.IO

import GHCJS.DOM (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (getBody, createElement, click)
import GHCJS.DOM.HTMLElement (setInnerText)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.HTMLParagraphElement (castToHTMLParagraphElement)
import GHCJS.DOM.Node (appendChild, insertBefore, getFirstChild)
import GHCJS.DOM.EventM (on, mouseClientXY)

import Data.Time.Clock

import Ef.Knot
import Ef.Knot.Methods

runClient :: IO ()
runClient = do
    let obj = Object $ knot *:* Empty
    obj $. do
        ct <- io getCurrentTime
        linearize $ produce +>> consumer ct
    return ()
    where
        {-# INLINE produce #-}
        produce initial = knotted $ \_ dn -> start dn initial
            where
                {-# INLINE start #-}
                start respond = go
                    where
                        {-# INLINE go #-}
                        go () = do
                            next <- respond ()
                            go next
        {-# INLINE consumer #-}
        consumer started = knotted $ \up _ -> start up
            where
                {-# INLINE start #-}
                start request = go (1000000 :: Int)
                    where
                        {-# INLINE go #-}
                        go 0 = do
                           ended <- io getCurrentTime
                           io (print $ show (diffUTCTime ended started))
                           return ()
                        go n = do
                           () <- request ()
                           go $! n - 1
{-
        mv <- io newEmptyMVar
        endMV <- io newEmptyMVar
        io $ runWebGUI $ \webView -> do
            enableInspector webView
            Just doc <- webViewGetDomDocument webView
            Just body <- getBody doc
            setInnerHTML body (Just "<h1>Hello World</h1>")
            start <- getCurrentTime
            forkIO (go doc body mv endMV)
            threadDelay 2000000
            putMVar mv ()
            end <- getCurrentTime
            count <- takeMVar endMV
            Just newParagraph <- fmap castToHTMLParagraphElement <$> createElement doc (Just "p")
            setInnerText newParagraph $ Just (show (diffUTCTime end start,count))
            firstChild <- getFirstChild body
            insertBefore body (Just newParagraph) firstChild
            return ()
    return ()
    where

        go doc body mv end = start (0 :: Int)
          where
              start n = do
                  took <- tryTakeMVar mv
                  case took of
                       Nothing -> do
                           Just newParagraph <- fmap castToHTMLParagraphElement <$> createElement doc (Just "p")
                           firstChild <- getFirstChild body
                           setInnerText newParagraph $ Just ""
                           insertBefore body (Just newParagraph) firstChild
                           start (n + 1)
                       Just _ -> putMVar end n
-}
