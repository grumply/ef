{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
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

import Ef.State
import Ef.State.Methods

import qualified Control.Exception as Exception
import Data.Functor.Identity
import Data.IORef
import System.IO.Unsafe

import Data.Function


run obj [Return result] = return (obj,result)
run obj (Fail e:_) = Exception.throw e
run obj [Super sup] = sup >>= \nar -> run obj $ deconstruct nar
run obj (Say message _:rest) = do
    (obj',value) <- delta obj (Say message Return)
    

runClient :: IO ()
runClient = do
    began <- getCurrentTime
    let (obj',result) = runIdentity $ run (Object $ state (0 :: Int) *:* Empty) $ fix go (1000000 :: Int)
        go continue n = 
            if n == 0 then return () else put n >> continue (n - 1)
    ended <- result `seq` getCurrentTime
    print (diffUTCTime ended began,result)
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
