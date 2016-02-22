{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Ef
import Ef.IO
import Ef.Event
import HTML
import Router

import Ease

import Templating.Simple
import Templating.Bootstrap

import GHCJS.DOM.Types (MouseEvent)
import GHCJS.DOM.Event
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.Window as W

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Typeable

import Unsafe.Coerce

--------------------------------------------------------------------------------
-- Helpers

dark = "#222222"
light = "white"


whyef = "Why Ef?"
features = "Features"
samples = "Samples"
getInvolved = "Get Involved"


sections = [whyef,features,samples,getInvolved]


clean = filter isAlpha . map toLower

--------------------------------------------------------------------------------
-- Global Attributes

hrStyles = do
    "height"        =: "2px"
    "border-top"    =: "1px solid"
    "border-bottom" =: "1px solid"
    "border-color"  =: "#f26f21"
    smallScreens
    largeScreens
    where

        smallScreens = maxWidth 767 $ do
            "max-width" =: "100px"
            "margin"    =: "15px auto"

        largeScreens = minWidth 768 $ do
            "max-width" =: "200px"
            "margin"    =: "20px auto"


--------------------------------------------------------------------------------
-- Navbar


navbar = do
    (nvbr,_) <- topFixedInverseNavbar $ do
        navbarStyles
        container $ do
            navHeader (hamburger >> navBrand)
            collapsableNavbar (navbarRight navElements)
    smartShow nvbr
    where
        smartShow nvbr = super $ do
            Event{..} <- Ef.Event.event return
            scrolls <- getScrollSignal
            resizes <- getResizeSignal
            scrollsAndResizes <- mergeSignals undefined scrolls resizes
            win <- getWindow
            behavior scrollsAndResizes (react win)
            where
                react win Reactor{..} = go False
                    where
                        -- hasn't run yet
                        go False ev = do
                            undefined
                        go True ev = do
                            (x,y) <- demand =<< (query $ const $ (,) <$> (W.getScrollY win) <*> (W.getInnerWidth win))
                            with nvbr $ navbarStylesConditional (fromIntegral x) (fromIntegral y)
                            return ()

        navElements = do
            hiddenScrollTop
            forM_ sections $ \section -> do
                pageLink (clean section) ("color" =: light)

        navBrand =
            brandedPageTop $ do
                "padding" =: "0 15px"
                "color"   =: light
                i <- super $ pngFile "ef-logo-color-small"
                _img "img-responsive" i simple

        navbarStyles = do
            "border" =: "none"
            w <- super getWindowWidth
            navbarStylesConditional w 0

        navbarStylesConditional w y
            | y > 50 || w < 768 = do
                  "background-color" =: dark
                  "margin-bottom"    =: "1px solid rgba(255,255,255,0.1)"
                  removeStyle "padding"
                  removeStyle "transition"

            | otherwise = do
                  "background-color" =: "transparent"
                  "padding"          =: "30px 0"
                  "transition"       =: "padding 0.5s"


--------------------------------------------------------------------------------
-- Header


intro = do
    (_,scrolls) <- child "header" $ do
        headerStyles
        quote
        scrollPageButton whyef
    return scrolls
    where

        headerStyles = do
            i <- url (jpgFile "ef-bg")
            "position"            =: "relative"
            "display"             =: "block"
            "background"          =: i
            "background-color"    =: dark
            "background-repeat"   =: "no-repeat"
            "background-position" =: "center"
            "background-size"     =: "cover"
            smallScreens
            notSmallScreens
            notLargeScreens
            largeScreens

            where
                smallScreens = maxWidth 767 $ do
                    "height" =: "auto"
                    "width"  =: "auto"

                notSmallScreens = minWidth 768 $ do
                    "height" =: "100%"
                    "width"  =: "100%"

                notLargeScreens = maxWidth 1024 $ do
                    "padding"               =: "100px 0"
                    "background-attachment" =: "scroll"

                largeScreens = minWidth 1025 $ do
                    "padding"               =: "0"
                    "background-attachment" =: "fixed"


quote =
    _div "intro-content" $ do
        attributes
        quoteImage
    where
        attributes = do
            "color"      =: "white"
            "text-align" =: "center"
            smallScreens
            largeScreens
            where

                smallScreens = maxWidth 767 $ do
                    "position" =: "relative"
                    removeStyle "margin"
                    removeStyle "width"
                    removeStyle "top"
                    removeStyle "left"
                    removeStyle "transform"

                largeScreens = minWidth 768 $ do
                    "width"      =: "50%"
                    "margin"     =: "auto"
                    "position"   =: "absolute"
                    "top"        =: "50%"
                    "left"       =: "50%"
                    "transform"  =: "translate(-50%,-50%)"

        quoteImage = do
            i <- super $ pngFile "darwin"
            _img "img-responsive" i imageAttributes
           where

               imageAttributes = do
                   smallScreens
                   largeScreens
                   defaultAttributes
                   where

                       defaultAttributes = do
                           "margin"    =: "0 auto"
                           "display"   =: "block"
                           "max-width" =: "100%"
                           "height"    =: "auto"

                       smallScreens = maxWidth 767 $
                           "max-height" =: "150px"

                       largeScreens = minWidth 768 $
                           "max-height" =: "300px"


scrollPageButton section = do
    (_,scrollFromIntroClicks) <- _div "scroll-down" $ do
        scrollDownAttributes
        (_,(clicks,_)) <-
            child "a" $ do
                buttonAttributes
                _i "fa fa-angle-down fa-fw" simple
                listen E.click
        return clicks
    return scrollFromIntroClicks
    where

        scrollDownAttributes = do
            "position"       =: "absolute"
            "width"          =: "100%"
            "bottom"         =: "20px"
            "text-align"     =: "center"

        buttonAttributes     = do
            "height"         =: "50px"
            "width"          =: "50px"
            "border"         =: "2px solid " ++ light
            "border-radius"  =: "100%"
            "line-height"    =: "50px"
            "padding"        =: "0"
            "letter-spacing" =: "normal"
            "color"          =: light
            "font-size"      =: "30px"
            "text-transform" =: "uppercase"
            "font-weight"    =: "900"
            "transition"     =: "all 0.5s ease"

--------------------------------------------------------------------------------
-- Why Ef?


efDesc = "Ef unifies Object-Oriented, Method-Oriented, API-Oriented, and Message-Oriented \
         \programming through a pure, immutable, and Functional interface. Ef will allow \
         \all of these design approaches to converge on the same implementation through \
         \extensive derivation mechanisms."


whyefSection = child "section" $ do
    identity "whyef"
    "height" =: "15000px"
    _div "container-fluid" $
        _div "row text-center" $
            _div "col-lg-12" $ do
                "visibility" =: "visible"
                header
                content
                child "hr" hrStyles
    where

        header =
            child "h1" $ do
                "text-transform" =: "uppercase"
                smallScreens
                largeScreens
                setText "Variable Design Philosophy"
            where

                smallScreens = maxWidth 481 $
                    "font-size" =: "2.25rem"

                largeScreens = minWidth 482 $
                    "font-size" =: "3.25rem"

        content =
            child "p" $ do
                "line-height" =: "1.5"
                smallScreens
                largeScreens
                setText efDesc
            where

                smallScreens = maxWidth 481 $
                    "font-size" =: "1.75rem"

                largeScreens = minWidth 482 $
                    "font-size" =: "2.25rem"


--------------------------------------------------------------------------------
-- Footer


footer = simple


--------------------------------------------------------------------------------
-- Site

scrollTo Element{..} win drawer (over :: Double) ease offset initialTime = do
    endY  <- E.getOffsetTop dom
    beginY <- fromIntegral <$> W.getScrollY win
    let endTime = initialTime + over
        endY' = endY + offset
        easePAP = ease beginY endY' over
    start endY' endTime easePAP (arrive drawer . Right) initialTime
    where

        start endY endTime easePAP buffer initialTime =
            go
            where
                go =
                    buffer $ \t -> do
                        if endTime <= t then
                            W.scrollTo win 0 $ round endY
                        else do
                            let newY = easePAP (t - initialTime)
                            print (t,newY)
                            W.scrollTo win 0 $ round newY
                            go

easeInOutExpoGoto clicks e overMilli offset = do
    drawer <- super getDrawer
    win <- super getWindow
    on clicks $ \ev -> do
        io $ preventDefault ev
        void $ write $ scrollTo e win drawer overMilli easeInOutExpo offset

eflangorg = html "article" $ do
    attributes
    navbar
    scrollFromIntroClicks <- intro
    (whyefElement,_) <- whyefSection
    footer
    easeInOutExpoGoto scrollFromIntroClicks whyefElement 1250 (-50)
    where

        attributes = do
            identity "page-top"
            viewStyles
            fontStyles

        viewStyles = do
            "height"      =: "100%"

        fontStyles = do
            "font-family" =: fonts ["Georgia","serif"]
            "font-weight" =: "400"
            "font-size"   =: "2rem"


site = do
    (content,_) <- eflangorg
    appendBody content


--------------------------------------------------------------------------------
-- Entry

main = routed

-- main :: IO ()
-- main = client Client{..} where
--     prime root = return (signals *:* assets *:* root,())
--     build _ = do
--         rs <- HTML.resizeSignal
--         ss <- HTML.scrollSignal
--         setResizeSignal rs
--         setScrollSignal ss
--         site
--     drive intrcptr _ = run
--         where
--             run obj = do
--                 (obj',_) <- obj $. intrcptr Blocking
--                 run obj'


--------------------------------------------------------------------------------
-- Assets attribute to ease asset filepath creation. If I like this
-- approach, it can be extracted.

-- | Assets represents a directory structure for a web application by
-- colocating directories under a common resource directory, e.g.
-- a font file may be found under `resourceDir </> fontDir`. Vendor files,
-- or external libraries, will be found under the `vendorDir`.
data Assets k
    = Assets
          { resourceDir :: FilePath
          , cssDir      :: FilePath
          , jsDir       :: FilePath
          , imageDir    :: FilePath
          , fontDir     :: FilePath
          , dataDir     :: FilePath
          , vendorDir   :: FilePath
          , getAssetDir      :: k
          }
    | GetResourceDir (FilePath -> k)
    | GetCSSDir      (FilePath -> k)
    | GetJSDir       (FilePath -> k)
    | GetImageDir    (FilePath -> k)
    | GetFontDir     (FilePath -> k)
    | GetDataDir     (FilePath -> k)
    | GetVendorDir   (FilePath -> k)
instance Ma Assets Assets where
  ma use Assets {..} (GetResourceDir fk) = use getAssetDir (fk resourceDir)
  ma use Assets {..} (GetCSSDir      fk) = use getAssetDir (fk cssDir)
  ma use Assets {..} (GetImageDir    fk) = use getAssetDir (fk imageDir)
  ma use Assets {..} (GetFontDir     fk) = use getAssetDir (fk fontDir)
  ma use Assets {..} (GetDataDir     fk) = use getAssetDir (fk dataDir)
  ma use Assets {..} (GetVendorDir   fk) = use getAssetDir (fk vendorDir)
assets =
    let resourceDir = "resources"
        cssDir = "css"
        jsDir = "js"
        imageDir = "img"
        fontDir = "font"
        dataDir = "data"
        vendorDir = "lib"
        getAssetDir = return
    in Assets {..}
getVendorDir = self (GetVendorDir id)
getResourceDir = self (GetResourceDir id)
createResource getAssetDir file ext = do
    rDir <- getResourceDir
    aDir <- getAssetDir
    return $ rDir ++ "/" ++ aDir ++ "/" ++ file ++ ext
cssFile  f = createResource (self (GetCSSDir   id)) f ".css"
jsFile   f = createResource (self (GetJSDir    id)) f ".js"
jpgFile  f = createResource (self (GetImageDir id)) f ".jpg"
pngFile  f = createResource (self (GetImageDir id)) f ".png"
gifFile  f = createResource (self (GetImageDir id)) f ".gif"
ttfFile  f = createResource (self (GetFontDir  id)) f ".ttf"
otfFile  f = createResource (self (GetFontDir  id)) f ".otf"
woffFile f = createResource (self (GetFontDir  id)) f ".woff"
dataFile f = createResource (self (GetDataDir  id)) f ""
vendorFile f = getVendorDir >>= \vd -> return $ vd ++ "/" ++ f


--------------------------------------------------------------------------------
-- Signals
