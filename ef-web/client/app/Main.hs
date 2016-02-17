{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Ef
import Ef.IO
import Ef.Event
import HTML

import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.Document as D


main :: IO ()
main = simple $ event $ \Event{..} -> do
    (divElem,(clicks,_)) <- html "div" $ do
        "height" =: "90px"
        "backgroundColor" =: "black"
        listen E.click
    doc <- getDocument
    Just body <- io $ D.getBody doc
    embed body divElem
    behavior clicks countClicks
    where
        countClicks Reactor{..} = go (0 :: Int)
            where
                go n _ = do
                    io $ print $ "Click " ++ show n
                    become (go $ n + 1)
