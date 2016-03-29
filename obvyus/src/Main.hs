{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language NoMonomorphismRestriction #-}
{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Ef
import Ef.Type.Set
import Ef.Single

import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.Types as T
import qualified GHCJS.DOM.KeyboardEvent as K
import qualified GHCJS.DOM.UIEvent as U

import qualified Control.Monad.Trans.Class as Trans

import Flowers

import Obvyus
import Menu
import Listing
import Listings

import Data.Promise

import Control.Monad
import Data.List
import Prelude

import Unsafe.Coerce

{-
Add generic event listening for hover

Consider allowing set and reset for pairs of events like onMouseIn/onMouseOut

-}

--------------------------------------------------------------------------------

app base = listings *:* listings *:* obvyus *:* mkMenu *:* base

type App = '[ Listings ProvocativeListings
            , Listings InterestingListings
            , Obvyus
            , Menu
            , Dahlia
            , Nelumbo
            , SingleKnot
            ]

--------------------------------------------------------------------------------

footerHeight = 3

timesNewRoman weight color size =
    font "\"Times New Roman\", Serif" weight color size

helveticaNeue weight color size =
    font "\"Helvetica Neue\",Helvetica,Arial" weight color size

--------------------------------------------------------------------------------

hat =
    div_ $ do
        style hatStyles
        hatLeft
        hatRight
    where

        hatStyles = do
            flexibleRow
            marginBottom =: px 6

        hatLeft =
            column_ 10 $ do
                style flexibleStart
                logo
                brand
                links
            where

                logo =
                    ahref_ "O" "" $ do
                        style brandLetterStyling
                    where

                        brandLetterStyling = do
                            textDecoration =: none
                            bgColor     =: darkcyan
                            padding     =: px2 2 5
                            marginRight =: px 10
                            timesNewRoman bold white (px 15)

                brand =
                    a_ $ do
                        href ""
                        style brandStyles
                        linkText "Obvy"
                        separator
                        linkText "us"
                    where

                        brandStyles = do
                            textDecoration =: none
                            marginRight    =: px 16

                        linkText txt =
                          span_ $ do
                              text txt
                              style $ helveticaNeue (weight 600) black (px 13)

                        separator =
                            span_ $ do
                                style separatorStyles
                                text "|"
                            where

                                separatorStyles = do
                                    helveticaNeue (weight 200) (rgba 0 0 0 0.5) (px 28)
                                    position =: relative
                                    top      =: px 4

                -- wrap in ul; make links li
                links = do
                    link "Interesting" "/interesting"
                    spacer
                    link "Provacative" "/provacative"
                    where

                        link txt lnk =
                            ahref__ txt lnk (txt ++ "Link") $ do
                                style navLinkStyles
                            where

                                navLinkStyles = do
                                    textDecoration =: none
                                    fontSize       =: px 14
                                    color          =: gray

                        spacer =
                            span_ $ do
                                style spacerStyles
                            where

                                spacerStyles = do
                                    margin =: px2 0 6

        -- right side containing login button
        hatRight =
            column_ 2 $ do
                style flexibleEnd
                loginLink
            where

                loginLink =
                    a_ $ do
                        style linkStyles
                        loginGlyph
                        fst <$> listen E.click (const ()) listenOpts
                    where

                        linkStyles = do
                            cursor =: pointer
                            marginRight =: px (-4)

                        loginGlyph =
                            span_ $ do
                                glyph gLogIn
                                style loginGlyphStyles
                            where

                                loginGlyphStyles = do
                                    top            =: px 8
                                    position       =: relative
                                    textDecoration =: none
                                    fontSize       =: px 20
                                    color          =: darkcyan

--------------------------------------------------------------------------------

divider =
    div_ $ do
        style dividerStyles
        column_ 1 (return ())
        column_ 10 $ hr_ (style ruleStyles)
        column_ 1 (return ())
    where

        dividerStyles = do
            flexibleRow

        ruleStyles = do
            border  =: zero
            height  =: px 1
            margin  =: px2 5 0
            bgImage =: "linear-gradient(to right,\
                       \  rgba(0,0,0,0),\
                       \  rgba(0,0,0,0.75),\
                       \  rgba(0,0,0,0)\
                       \)"

--------------------------------------------------------------------------------

footer =
    div_ $ do
        style footerStyles
        column_ 12 $ do
            style flexibleCenter
            footerNav
        column_ 12 $ do
            style flexibleCenter
            copyright
    where

        footerStyles = do
            flexibleRow

        footerNav =
            unorderedList_ (style footerListStyles) $ do
                intersperse footerSeparator
                    [ footerLink "About" "/about"
                    , footerLink "Contact" "/contact"
                    , footerLink "Privacy" "/privacy"
                    ]
            where

                footerListStyles = do
                    listStyle =: none
                    margin    =: zero
                    padding   =: zero

                footerLink txt lnk = do
                    style $ display =: inline
                    void $ ahref_ txt lnk $ do
                        style footerLinkStyles
                    where

                        footerLinkStyles = do
                            textDecoration =: none
                            color          =: gray
                            fontSize       =: px 12

                footerSeparator = do
                    style $ display =: inline
                    void $ span_ $ do
                        style footerSeparatorStyles
                        text "|"
                    where

                        footerSeparatorStyles = do
                            color    =: gray
                            fontSize =: px 12
                            margin   =: px2 0 10

        copyright =
            span_ $ do
                text "© 2016 S. M. Hickman"
                style copyrightStyles
            where

                copyrightStyles = do
                    color    =: gray
                    fontSize =: px 12

--------------------------------------------------------------------------------

content :: String
content = "content"

contentContainer = div__ content (return ())

loginModal =
    div__ "LoginModal" $ do
        style loginModalStyles
        closeButton
    where

        closeButton =
            fst <$> (span_ $ do
                text "X"
                style closeButtonStyles
                listen E.click (const ()) listenOpts)
            where

                closeButtonStyles = do
                    color      =: hex 0xaaa
                    float      =: right
                    fontSize   =: px 28
                    fontWeight =: bold

        loginModalStyles = do
            display  =: none
            position =: fixed
            zIndex   =: one
            left     =: zero
            top      =: zero
            width    =: per 100
            height   =: per 100
            overflow =: auto
            bgColor  =: rgb 0 0 0
            bgColor  +: rgba 0 0 0 0.4

loginModalHandler :: Signal App IO ()
                  -> Signal App IO ()
                  -> Narrative '[Camellia] (Narrative App IO) ()
loginModalHandler loginClicks modalCloseClicks = void $
    behavior loginClicks $ \_ ev -> do
        (escPresses,removeEscListener) <- keyUp 27 listenOpts
        (closeSignal,unregEscs,unregCloses) <- mergeSignals' escPresses modalCloseClicks
        with "LoginModal" $ style $ display =: block
        void $ behavior' closeSignal $ \r _ -> do
            with "LoginModal" $ style $ display =: none
            removeEscListener
            unregCloses
            unregEscs
            (die r)

--------------------------------------------------------------------------------

layout =
    void $ with lotus $ do
        style lotusStyles
        pageMain
        pageBottom

    where

        lotusStyles = do
            height =: per 100
            flexibleContainer

        pageMain =
            div_ $ do
                style pageMainStyles
                loginClicks <- hat
                divider
                modalCloseClicks <- loginModal
                contentContainer
                loginModalHandler loginClicks modalCloseClicks
                pushDiv
                return loginClicks
            where
                pushDiv =
                    child "div" Nothing $ do
                        style $ height =: ems 4


                pageMainStyles = do
                    minHeight =: per 100
                    height    =: render auto important
                    height    +: per 100
                    margin    =: render zero auto (ems $ negate footerHeight)


        pageBottom =
            div_ $ do
                style pageBottomStyles
                divider
                footer
            where

               pageBottomStyles = do
                   flexibleContainer
                   height =: ems footerHeight

--------------------------------------------------------------------------------

changeMenuHighlights iColor pColor =
    super $ do
        with "InterestingLink" $ change $ color =: iColor
        with "ProvacativeLink" $ change $ color =: pColor

--------------------------------------------------------------------------------

pg :: String
   -> String
   -> Narrative '[Camellia] (Narrative App IO) ()
   -> Narrative '[Rosa] (Narrative App IO) ()
pg i p c =
    dispatch $ with content $ do
        changeMenuHighlights i p
        deleteChildren
        c

interesting = pg black gray
provacative = pg gray  black
about       = pg gray  gray
contact     = pg gray  gray
privacy     = pg gray  gray

--------------------------------------------------------------------------------

main :: IO ()
main = run Config{..}
    where
        routes = do
            path "/interesting" $ interesting $ do
                return ()

            path "/about" $ about $ do
                return ()

            path "/contact" $ contact $ do
                return ()

            path "/privacy" $ privacy $ do
                return ()

            -- default
            provacative $ do
                return ()

        prime base = return (app base)

        build = layout
