{-# language RecordWildCards #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}
{-# language NoMonomorphismRestriction #-}
{-# language PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Menu where

import Ef
import Ef.Event

import Dahlia
import Alyssum

import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.Types as T

import Control.Monad
import Data.List

import Unsafe.Coerce

import Prelude

data LoginWidget k
    = LoginWidget
          { _displayingLoggedIn :: (Bool,k)
          , _displayLoggedIn :: k
          , _displayNotLoggedIn :: k
          , _loginWidgetNode :: (Node,k)
          , _loginClicks :: forall self super. (Signal self super T.MouseEvent,k)
          , _setLoginClicks :: forall self super. Signal self super T.MouseEvent -> k
          , _setLoginWidgetNode :: Node -> k
          , _loggedInWidgetNode :: (Node,k)
          , _setLoggedInWidgetNode :: Node -> k
          }

data Menu k
    = Menu
          { _loginWidget       :: LoginWidget k
          }
    | DisplayingLoggedIn (Bool -> k)
    | DisplayLoggedIn k
    | DisplayNotLoggedIn k
    | forall self super. LoginClicks (Signal self super T.MouseEvent -> k)
    | forall self super. SetLoginClicks (Signal self super T.MouseEvent) k
    | LoginWidgetNode (Node -> k)
    | SetLoginWidgetNode Node k
    | LoggedInWidgetNode (Node -> k)
    | SetLoggedInWidgetNode Node k

displayingLoggedIn = self (DisplayingLoggedIn id)

displayLoggedIn = self (DisplayLoggedIn ())

displayNotLoggedIn = self (DisplayNotLoggedIn ())

loginClicks :: (Monad super, '[Menu] <: self)
            => Narrative self super (Signal self super T.MouseEvent)
loginClicks = self (LoginClicks id)

setLoginClicks :: (Monad super ,'[Menu] <: self)
               => Signal self super T.MouseEvent -> Narrative self super ()
setLoginClicks c = self (SetLoginClicks c ())

loginWidgetNode = self (LoginWidgetNode id)

setLoginWidgetNode n = self (SetLoginWidgetNode n ())

loggedInWidgetNode = self (LoggedInWidgetNode id)

setLoggedInWidgetNode n = self (SetLoggedInWidgetNode n ())


instance Ma (Menu) (Menu) where
    ma use Menu{..} (DisplayingLoggedIn bk) =
        use (snd $ _displayingLoggedIn _loginWidget)
            (bk $ fst $ _displayingLoggedIn _loginWidget)
    ma use Menu{..} (DisplayLoggedIn k) =
        use (_displayLoggedIn _loginWidget) k
    ma use Menu{..} (DisplayNotLoggedIn k) =
        use (_displayNotLoggedIn _loginWidget) k
    ma use Menu{..} (LoginClicks lk) =
        use (snd $ _loginClicks _loginWidget)
            (lk $ fst $ _loginClicks _loginWidget)
    ma use Menu{..} (SetLoginClicks lc k) =
        use (_setLoginClicks _loginWidget $ lc) k
    ma use Menu{..} (LoginWidgetNode nk) =
        use (snd $ _loginWidgetNode _loginWidget)
            (nk $ fst $ _loginWidgetNode _loginWidget)
    ma use Menu{..} (SetLoginWidgetNode n k) =
        use (_setLoginWidgetNode _loginWidget $ n) k
    ma use Menu{..} (LoggedInWidgetNode nk) =
        use (snd $ _loggedInWidgetNode _loginWidget)
            (nk $ fst $ _loggedInWidgetNode _loginWidget)
    ma use Menu{..} (SetLoggedInWidgetNode n k) =
        use (_setLoggedInWidgetNode _loginWidget $ n) k

mkMenu :: forall self super methods.
        (Ma (Methods methods) (Messages self), Monad super, '[Menu] <: self, Subclass '[Menu] methods)
     => Menu (Implementation methods super)
mkMenu = Menu
    { _loginWidget = LoginWidget
            { _displayingLoggedIn = (False,return)
            , _displayLoggedIn = return
            , _displayNotLoggedIn = return
            , _loginClicks = (undefined,return)
            , _setLoginClicks = \lc fs ->
                    let m = view fs
                        lw = _loginWidget m
                    in return $ fs .= m { _loginWidget = lw { _loginClicks = (unsafeCoerce lc, snd $ _loginClicks lw) } }
            , _loginWidgetNode = (undefined,return)
            , _setLoginWidgetNode = \n fs ->
                    let m = view fs
                        lw = _loginWidget m
                    in return $ fs .= m { _loginWidget = lw { _loginWidgetNode = (n, snd $ _loginWidgetNode lw) } }
            , _loggedInWidgetNode = (undefined,return)
            , _setLoggedInWidgetNode = \n fs ->
                    let m = view fs
                        lw = _loginWidget m
                    in return $ fs .= m { _loginWidget = lw { _loggedInWidgetNode = (n, snd $ _loggedInWidgetNode lw) } }
            }
    }

--------------------------------------------------------------------------------

timesNewRoman weight color size =
    font "\"Times New Roman\", Serif" weight color size

helveticaNeue weight color size =
    font "\"Helvetica Neue\",Helvetica,Arial" weight color size

brandLetterStyling = do
    bgColor     =: darkcyan
    padding     =: px2 2 5
    marginRight =: px 10

navLinkStyles = do
    textDecoration =: none
    fontSize       =: px 14
    color          =: gray


--------------------------------------------------------------------------------

logo = ahref_ "O" "" $ style $ do
    textDecoration =: none
    brandLetterStyling
    timesNewRoman bold white (px 15)

--------------------------------------------------------------------------------

brand = a_ $ do
    href ""
    style $ do
        textDecoration =: none
        marginRight    =: px 16
    linkText "Obvy"
    separator
    linkText "us"
    where

        linkText txt = span_ $ do
            text txt
            style $ helveticaNeue (weight 600) black (px 13)

        separator = span_ $ do
            text "|"
            style $ do
                helveticaNeue (weight 200) (rgba 0 0 0 0.5) (px 28)
                position =: relative
                top      =: px 4

--------------------------------------------------------------------------------

links = do
    ahref__ "Interesting" "/interesting" "InterestingLink" $
        style navLinkStyles
    span_ $ style $ margin =: px2 0 6
    ahref__ "Provacative" "/provacative" "ProvacativeLink" $
        style navLinkStyles

--------------------------------------------------------------------------------

loginLink = a_ $ do
    href "/login"
    style $ marginRight =: px (-4)
    span_ $ do
        style loginGlyphStyles
        glyph gLogIn
    where

        loginGlyphStyles = do
            top            =: px 8
            position       =: relative
            textDecoration =: none
            fontSize       =: px 20
            color          =: darkcyan

--------------------------------------------------------------------------------

hat = do
    div_ $ do
        style $ do
            flexibleRow
            marginBottom =: px 4
        hatLeft
        hatRight
    divider
    where

        -- left side containing branding and navigation
        hatLeft = column_ 10 $ do
            style flexibleStart
            logo
            brand
            links

        -- right side containing login button
        hatRight = column_ 2 $ do
            style flexibleEnd
            loginLink

--------------------------------------------------------------------------------

divider = do
    div_ $ do
        style flexibleRow
        column_ 1 (return ())
        column_ 10 $ hr_ (style dividerStyles)
        column_ 1 (return ())

dividerStyles = do
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
        divider
        div_ $ do
            style $ do
                flexibleRow
                flexibleMiddle
            column_ 12 $ do
                style flexibleCenter
                unorderedList_ (style footerListStyles) $ do
                    intersperse footerSeparator
                        [ footerLink "About" "/about"
                        , footerLink "Contact" "/contact"
                        , footerLink "Privacy" "/privacy"
                        ]
            column_ 12 $ do
                style flexibleCenter
                span_ $ do
                    style $ do
                        color    =: gray
                        fontSize =: px 12
                    text "© 2016 S. M. Hickman"
    where

        footerStyles = do
            height   =: ems 4

        footerListStyles = do
            listStyle =: none
            margin    =: zero
            padding   =: zero

        footerLinkStyles = do
            textDecoration =: none
            color          =: gray
            fontSize       =: px 12

        footerLink txt lnk = do
            style $ display =: inline
            void $ ahref_ txt lnk $ style footerLinkStyles

        footerSeparatorStyles = do
            color    =: gray
            fontSize =: px 12
            margin   =: px2 0 10

        footerSeparator = do
            style $ display =: inline
            void $ span_ $ do
                style footerSeparatorStyles
                text "|"
